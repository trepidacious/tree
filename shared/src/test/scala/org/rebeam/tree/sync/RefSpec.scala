package org.rebeam.tree.sync

import io.circe.generic.JsonCodec
import org.rebeam.lenses.macros.Lenses
import org.rebeam.tree.Delta._
import org.rebeam.tree.DeltaCodecs._
import org.rebeam.tree.sync.Sync._
import org.rebeam.tree._
import org.scalatest._
import org.scalatest.prop.Checkers

import scala.language.higherKinds
import org.rebeam.tree.BasicDeltaDecoders._
import org.rebeam.tree.sync.Sync.Ref._

class RefSpec extends WordSpec with Matchers with Checkers {

  case class SimpleCacheState[A](data: A, revision: Long, incomingRefs: Set[Guid[_]], outgoingRefs: Set[Guid[_]])

  class SimpleCache[M](private val map: Map[Guid[_], SimpleCacheState[_]])(implicit dCodecM: DeltaCodec[M]) extends Cache {

    def apply[A](ref: Ref[A]): Option[A] = ref match {
      case RefUnresolved(_) => None
      case RefResolved(guid, revision) => getState(guid).filter(_.revision == revision).map(_.data)
    }

    def updateRef[A](ref: Ref[A]): Option[Ref[A]] = getState(ref.guid).map(_.revision).filterNot(ref.optionRevision.contains(_)).map(RefResolved(ref.guid, _))

    private def getState[A](id: Guid[A]): Option[SimpleCacheState[A]] = map.get(id).map(_.asInstanceOf[SimpleCacheState[A]])

    private def get[A](id: Guid[A]): Option[A] = getState(id).map(_.data)

    //TODO handle refsTo and From

    //TODO update new entry?
    def updated[A <: M](id: Guid[A], a: A): SimpleCache[M] = {
      val updatedRev = getState(id).map(_.revision + 1).getOrElse(0L)
      val rur = dCodecM.updateRefs(RefUpdateResult.noRefs(a), this)
      new SimpleCache(map.updated(id, SimpleCacheState(rur.data, updatedRev, Set.empty, rur.outgoingRefs)))
    }

    def updated[A <: M](a: A)(implicit toId: ToId[A]): SimpleCache[M] = updated(toId.id(a), a)

    override def toString: String = "SimpleCache(" + map + ")"
  }

  object SimpleCache {
    def empty[M: DeltaCodec] = new SimpleCache[M](Map.empty)
  }

  @JsonCodec
  sealed trait Data

  object Data {
    @JsonCodec
    @Lenses
    case class Address(id: Guid[Address], streetName: String, number: Int) extends HasId[Address] with Data

    @JsonCodec
    @Lenses
    case class User(id: Guid[User], name: String, addressRef: Ref[Address]) extends HasId[User] with Data

    @JsonCodec
    @Lenses
    case class Post(id: Guid[Post], message: String, userRef: Ref[User], tagRefs: List[Ref[Tag]]) extends HasId[Post] with Data

    @JsonCodec
    @Lenses
    case class Tag(id: Guid[Tag], tag: String) extends HasId[Tag] with Data
  }

  import Data._

  implicit lazy val addressDeltaDecoder =
    DeltaCodecs.value[Address] or lensN(Address.streetName) or lensN(Address.number)

  implicit lazy val userDeltaDecoder =
    DeltaCodecs.value[User] or lensN(User.name) or lensN(User.addressRef)

  implicit lazy val postDeltaDecoder: DeltaCodec[Post] =
    DeltaCodecs.value[Post] or lensN(Post.message) or lensN(Post.userRef) or lensN(Post.tagRefs)

  implicit lazy val tagDeltaDecoder =
    DeltaCodecs.value[Tag] or lensN(Tag.tag)

  implicit lazy val listTagRefsDecoder = DeltaCodecs.optionalI[Ref[Tag]]

  implicit lazy val dataDeltaCodec = DeltaCodecs.value[Data]


  val examplePostIO: DeltaIO[(Post, SimpleCache[Data])] = for {
    addressId <- getId[Address]
    userId <- getId[User]
    postId <- getId[Post]
    tagId1 <- getId[Tag]
    tagId2 <- getId[Tag]
  } yield {
    val post = Post(postId, "Hello World!", Ref(userId), List(Ref(tagId1), Ref(tagId2)))
    (
      post,
      SimpleCache.empty[Data]
        .updated(Address(addressId, "Street Name", 42))
        .updated(User(userId, "User", Ref(addressId)))
        .updated(post)
        .updated(Tag(tagId1, "Tag1"))
        .updated(Tag(tagId2, "Tag2"))
    )
  }

  val examplePost: (Post, SimpleCache[Data]) = DeltaIORun.runDeltaIO(examplePostIO, DeltaIOContext(Moment(100)), DeltaId(ClientId(1), ClientDeltaId(1)))

  "DeltaCodecs" should {
    "update refs" in {
      val post = examplePost._1
      val postUpdated = postDeltaDecoder.updateRefsDataOnly(examplePost._1, examplePost._2)

      //All refs should be updated to rev 0
      val postExpected = Post(
        id = post.id,
        message = post.message,
        userRef = RefResolved(post.userRef.guid, 0),
        tagRefs = List(
          RefResolved(post.tagRefs(0).guid, 0),
          RefResolved(post.tagRefs(1).guid, 0)
        )
      )

      assert(postUpdated == postExpected)
      println(postUpdated)
    }

    "update refs incrementally" in {
      val post = examplePost._1
      val cache = examplePost._2

      //Check the un-updated post's user ref doesn't get data
      assert(cache(post.userRef).isEmpty)

      //Update the post's refs so they have revisions
      val postUpdated = postDeltaDecoder.updateRefsDataOnly(examplePost._1, examplePost._2)

      // Get the user from the cache using updated ref having revision
      val user = cache(postUpdated.userRef).get

      // Update the user data, and update the cache with thi data
      val userUpdated = user.copy(name = "UserModified")
      val cacheUpdated = cache.updated(userUpdated)

      //Now update the post's refs again with the updated cache
      val postUpdated2 = postDeltaDecoder.updateRefsDataOnly(postUpdated, cacheUpdated)

      //User ref should be updated to rev 1, tags still at 0
      val postExpected = Post(
        id = post.id,
        message = post.message,
        userRef = RefResolved(post.userRef.guid, 1),
        tagRefs = List(
          RefResolved(post.tagRefs(0).guid, 0),
          RefResolved(post.tagRefs(1).guid, 0)
        )
      )
      assert(postUpdated2 == postExpected)

      // And we can still get at the user in the updated cache, but only with the corresponding updated post
      assert(cacheUpdated(post.userRef).isEmpty)
      assert(cacheUpdated(postUpdated.userRef).isEmpty)
      assert(cacheUpdated(postUpdated2.userRef).contains(userUpdated))

    }

  }

}
