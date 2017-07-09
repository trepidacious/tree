package org.rebeam.tree.sync

import io.circe.generic.JsonCodec
import org.rebeam.lenses.PrismN
import org.rebeam.lenses.macros.Lenses
import org.rebeam.tree.Delta._
import org.rebeam.tree.DeltaCodecs._
import org.rebeam.tree.sync.Sync._
import org.rebeam.tree._
import org.scalatest._
import org.scalatest.prop.Checkers

import scala.language.higherKinds
import org.rebeam.tree.BasicDeltaDecoders._
import org.rebeam.tree.ref.Cache
import org.rebeam.tree.ref.Ref._
import org.rebeam.tree.ref.Ref

class RefSpec extends WordSpec with Matchers with Checkers {

  @JsonCodec
  sealed trait Data extends HasId[Data]

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

    implicit val addressPrism: PrismN[Data, Address] = PrismN.classTag("Address")
    implicit val userPrism: PrismN[Data, User] = PrismN.classTag("User")
    implicit val postPrism: PrismN[Data, Post] = PrismN.classTag("Post")
    implicit val tagPrism: PrismN[Data, Tag] = PrismN.classTag("Tag")
  }

  import Data._

  implicit lazy val addressDeltaDecoder: DeltaCodec[Address] =
    DeltaCodecs.value[Address] or lensN(Address.streetName) or lensN(Address.number)

  implicit lazy val userDeltaDecoder: DeltaCodec[User] =
    DeltaCodecs.value[User] or lensN(User.name) or lensN(User.addressRef)

  implicit lazy val postDeltaDecoder: DeltaCodec[Post] =
    DeltaCodecs.value[Post] or lensN(Post.message) or lensN(Post.userRef) or lensN(Post.tagRefs)

  implicit lazy val tagDeltaDecoder: DeltaCodec[Tag] =
    DeltaCodecs.value[Tag] or lensN(Tag.tag)

  implicit lazy val listTagRefsDecoder: DeltaCodec[List[Ref[Tag]]] =
    DeltaCodecs.optionalI[Ref[Tag]]

  implicit lazy val dataDeltaCodec: DeltaCodec[Data] =
    prismN(Data.postPrism) or prismN(Data.addressPrism) or prismN(Data.userPrism) or prismN(Data.tagPrism)


  val examplePostIO: DeltaIO[(Post, Cache[Data])] = for {
    addressId <- getId[Address]
    userId <- getId[User]
    postId <- getId[Post]
    tagId1 <- getId[Tag]
    tagId2 <- getId[Tag]
  } yield {
    val post = Post(
      id = postId,
      message = "Hello World!",
      userRef = Ref(userId),
      tagRefs = List(Ref(tagId1), Ref(tagId2)))
    val cache = Cache.empty[Data]
      .updated(Address(addressId, "Street Name", 42))
      .updated(User(userId, "User", Ref(addressId)))
      .updated(post)
      .updated(Tag(tagId1, "Tag1"))
      .updated(Tag(tagId2, "Tag2"))
    (
      //FIXME get
      cache.getPrism(postId).get,
      cache
    )
  }

  val examplePost: (Post, Cache[Data]) = DeltaIORun.runDeltaIO(examplePostIO, DeltaIOContext(Moment(100)), DeltaId(ClientId(1), ClientDeltaId(1)))

  "Cache" should {
    "produce correct refs graph" in {
      val cache = examplePost._2
      val post = examplePost._1

      assert(cache.incomingRefs(post.id) == Set.empty)
      assert(cache.outgoingRefs(post.id) == Set(post.userRef.guid) ++ post.tagRefs.map(_.guid))

      val r = for {
        user <- cache.prism(post.userRef)
        address <- cache.prism(user.addressRef)
        tag1 <- cache.prism(post.tagRefs.head)
        tag2 <- cache.prism(post.tagRefs(1))
      } yield {
        assert(cache.incomingRefs(user.id) == Set(post.id))
        assert(cache.outgoingRefs(user.id) == Set(address.id))

        assert(cache.incomingRefs(address.id) == Set(user.id))
        assert(cache.outgoingRefs(address.id) == Set())

        assert(cache.incomingRefs(tag1.id) == Set(post.id))
        assert(cache.outgoingRefs(tag1.id) == Set())

        assert(cache.incomingRefs(tag2.id) == Set(post.id))
        assert(cache.outgoingRefs(tag2.id) == Set())
        true
      }

      assert(r.isDefined)
    }
  }

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
          RefResolved(post.tagRefs.head.guid, 0),
          RefResolved(post.tagRefs(1).guid, 0)
        )
      )

      assert(postUpdated == postExpected)
    }

    "find outgoing refs" in {
      val post = examplePost._1
      val rur = postDeltaDecoder.updateRefs(RefUpdateResult.noRefs(post), examplePost._2)

      assert(rur.outgoingRefs == Set(post.userRef.guid) ++ post.tagRefs.map(_.guid))
    }

    "find outgoing refs via prism" in {
      val post = examplePost._1
      val rur = dataDeltaCodec.updateRefs(RefUpdateResult.noRefs(post), examplePost._2)
      assert(rur.outgoingRefs == Set(post.userRef.guid) ++ post.tagRefs.map(_.guid))
    }

    "update refs incrementally" in {
      val post = examplePost._1
      val cache = examplePost._2

      //Update the post's refs so they have revisions
      val postUpdated = postDeltaDecoder.updateRefsDataOnly(examplePost._1, examplePost._2)

      // Get the user from the cache using updated ref having revision
      val user = cache.prism(postUpdated.userRef).get

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

      // And we can still get at the user in the updated cache, even with older refs
      assert(cacheUpdated(post.userRef).contains(userUpdated))
      assert(cacheUpdated(postUpdated.userRef).contains(userUpdated))
      assert(cacheUpdated(postUpdated2.userRef).contains(userUpdated))

    }

  }

}
