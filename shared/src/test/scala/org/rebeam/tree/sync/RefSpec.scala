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
import org.rebeam.tree.sync.Sync.Ref._

class RefSpec extends WordSpec with Matchers with Checkers {

  case class SimpleCacheState[A](data: A, revision: Long, incomingRefs: Set[Guid[_]], outgoingRefs: Set[Guid[_]]) {
    def updatedIncomingRefs(id: Guid[_], add: Boolean): SimpleCacheState[A] = copy(
      incomingRefs = if (add) {
        incomingRefs + id
      } else {
        incomingRefs - id
      }
    )
  }

  class SimpleCache[M](private val map: Map[Guid[_], SimpleCacheState[_]])(implicit dCodecM: DeltaCodec[M]) extends Cache {

    def apply[A](ref: Ref[A]): Option[A] = ref match {
      case RefUnresolved(_) => None
      case RefResolved(guid, revision) => getState(guid).filter(_.revision == revision).map(_.data)
    }

    def updateRef[A](ref: Ref[A]): Option[Ref[A]] = getState(ref.guid).map(_.revision).filterNot(ref.optionRevision.contains(_)).map(RefResolved(ref.guid, _))

    private def getState[A](id: Guid[A]): Option[SimpleCacheState[A]] = map.get(id).map(_.asInstanceOf[SimpleCacheState[A]])

    def incomingRefs[A](id: Guid[A]): Set[Guid[_]] = getState(id).map(_.incomingRefs).getOrElse(Set.empty[Guid[_]])
    def outgoingRefs[A](id: Guid[A]): Set[Guid[_]] = getState(id).map(_.outgoingRefs).getOrElse(Set.empty[Guid[_]])

    def get[A](id: Guid[A]): Option[A] = getState(id).map(_.data)

    private def incomingRefsFor(id: Guid[_]): Set[Guid[_]] = {
      map.foldLeft(Set.empty[Guid[_]]){
        case (refs, entry) =>
          // If the entry represents data having an outgoing ref to our id, then
          // add that data's id to the incoming refs.
          // Ignore the outgoing refs of our own data
          if (entry._2.outgoingRefs.contains(id) && entry._1 != id) {
            refs + entry._1
          // No outgoing ref from the entry's data to our id, so leave refs unaltered
          } else {
            refs
          }
      }
    }

    private def updateIncomingRefs(id: Guid[_], outgoingRefs: Set[Guid[_]], add: Boolean): SimpleCache[M] = {
      val map2 = outgoingRefs.foldLeft(map){
        case (m, outgoingRef) =>
          m.get(outgoingRef).fold {
            // If data reached by outgoing ref is not in cache, nothing to update
            m
          }{
            // If data IS in the cache, we update its cache state to add/remove ourselves as an incoming ref
            (otherCacheState: SimpleCacheState[_]) =>
              m.updated(
                outgoingRef,
                otherCacheState.updatedIncomingRefs(id, add)
              )
          }
      }
      new SimpleCache[M](map2)
    }

    def updated[A <: M](id: Guid[A], a: A): SimpleCache[M] = getState(id).fold {
      // TODO we can use the same map2 and updateIncomingRefs calls in both cases
      // if we produce the right values for updatedRev, previous/new outgoing refs
      // and previous incoming refs

      // We are adding new data - start from revision 0
      val updatedRev = 0L

      // Update refs in the added data
      val rur = dCodecM.updateRefs(RefUpdateResult.noRefs(a), this)

      // Data is new to the cache, so we need to update incoming refs from scratch,
      // and we need to add the new data to the incoming refs of all referenced data
      // that is actually in the cache
      val incomingRefs = incomingRefsFor(id)
      val map2 = map.updated(id, SimpleCacheState(rur.data, updatedRev, incomingRefs, rur.outgoingRefs))
      val cache2 = new SimpleCache[M](map2).updateIncomingRefs(id, rur.outgoingRefs, add = true)

      // Finally, we need to update the refs in everything that points at us.
      // This doesn't trigger any further updates, since the actual data in those
      // data items that point at us hasn't changed.
      incomingRefs.foldLeft(cache2){
        case (c, incomingId) =>
          c.getState(incomingId).fold(
            c
          )(
            state => {
              val rur = dCodecM.updateRefs(RefUpdateResult.noRefs(state.data.asInstanceOf[M]), c)
              new SimpleCache[M](c.map.updated(incomingId, state.copy(data = rur.data)))
            }
          )
      }

    }{
      // We are updating existing data
      previousState => {
        val updatedRev = previousState.revision + 1
        // Update refs in the updated data
        val updateResult = dCodecM.updateRefs(RefUpdateResult.noRefs(a), this)

        // Update the map with the data having updated refs, and new revision.
        // The incoming refs to this data have not changed since no other data has changed, so reuse from
        // current state.
        // The outgoing refs are as we just found.
        val incomingRefs = previousState.incomingRefs
        val map2 = map.updated(id, SimpleCacheState(updateResult.data, updatedRev, incomingRefs, updateResult.outgoingRefs))

        // Update the incoming refs of everything else in the cache. We can optimise by looking
        // at those outgoing refs that have been added to this data, and adding the corresponding
        // incoming references for the referenced data. Then similarly for outgoing references
        // that have been removed.
        val previousOutgoingRefs = previousState.outgoingRefs
        val newOutgoingRefs = updateResult.outgoingRefs
        val cache2 = new SimpleCache[M](map2)
          .updateIncomingRefs(id, newOutgoingRefs -- previousOutgoingRefs, add = true)
          .updateIncomingRefs(id, previousOutgoingRefs -- newOutgoingRefs, add = false)

        // Finally, we need to update the refs in everything that points at us.
        // This doesn't trigger any further updates, since the actual data in those
        // data items that point at us hasn't changed.
        incomingRefs.foldLeft(cache2){
          case (c, incomingId) =>
            c.getState(incomingId).fold(
              c
            )(
              state => {
                val rur = dCodecM.updateRefs(RefUpdateResult.noRefs(state.data.asInstanceOf[M]), c)
                new SimpleCache[M](c.map.updated(incomingId, state.copy(data = rur.data)))
              }
            )
        }

      }
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

    val addressPrism: PrismN[Data, Address] = PrismN.classTag("Address")
    val userPrism: PrismN[Data, User] = PrismN.classTag("User")
    val postPrism: PrismN[Data, Post] = PrismN.classTag("Post")
    val tagPrism: PrismN[Data, Tag] = PrismN.classTag("Tag")
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


  val examplePostIO: DeltaIO[(Post, SimpleCache[Data])] = for {
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
    val cache = SimpleCache.empty[Data]
      .updated(Address(addressId, "Street Name", 42))
      .updated(User(userId, "User", Ref(addressId)))
      .updated(post)
      .updated(Tag(tagId1, "Tag1"))
      .updated(Tag(tagId2, "Tag2"))
    (
      //FIXME get
      cache.get(postId).get,
      cache
    )
  }

  val examplePost: (Post, SimpleCache[Data]) = DeltaIORun.runDeltaIO(examplePostIO, DeltaIOContext(Moment(100)), DeltaId(ClientId(1), ClientDeltaId(1)))

  "SimpleCache" should {
    "produce correct refs graph" in {
      val cache = examplePost._2
      val post = examplePost._1

      assert(cache.incomingRefs(post.id) == Set.empty)
      assert(cache.outgoingRefs(post.id) == Set(post.userRef.guid) ++ post.tagRefs.map(_.guid))

      val r = for {
        user <- cache(post.userRef)
        address <- cache(user.addressRef)
        tag1 <- cache(post.tagRefs.head)
        tag2 <- cache(post.tagRefs(1))
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

//      //Check the un-updated post's user ref doesn't get data
//      assert(cache(post.userRef).isEmpty)

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