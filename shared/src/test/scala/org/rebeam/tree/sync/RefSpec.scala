package org.rebeam.tree.sync

import io.circe.{Decoder, Encoder}
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
import org.rebeam.tree.ref.{Mirror, MirrorCodec, Ref}

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
    case class Tag(id: Guid[Tag], tag: String) extends HasId[Tag] with Data

    @JsonCodec
    @Lenses
    case class Post(id: Guid[Post], message: String, userRef: Ref[User], tagRefs: List[Ref[Tag]]) extends HasId[Post] with Data

//    implicit val addressPrism: PrismN[Data, Address] = PrismN.classTag("Address")
//    implicit val userPrism: PrismN[Data, User] = PrismN.classTag("User")
//    implicit val postPrism: PrismN[Data, Post] = PrismN.classTag("Post")
//    implicit val tagPrism: PrismN[Data, Tag] = PrismN.classTag("Tag")
  }

  import Data._

  implicit val addressDeltaDecoder: DeltaCodec[Address] =
    DeltaCodecs.value[Address] or lensN(Address.streetName) or lensN(Address.number)

  implicit val tagDeltaDecoder: DeltaCodec[Tag] =
    DeltaCodecs.value[Tag] or lensN(Tag.tag)

  implicit val listTagRefsDecoder: DeltaCodec[List[Ref[Tag]]] =
    DeltaCodecs.optionalI[Ref[Tag]]

  implicit val userDeltaDecoder: DeltaCodec[User] =
    DeltaCodecs.value[User] or lensN(User.name) or lensN(User.addressRef)

  implicit val postDeltaDecoder: DeltaCodec[Post] =
    DeltaCodecs.value[Post] or lensN(Post.message) or lensN(Post.userRef) or lensN(Post.tagRefs)

  //  implicit lazy val dataDeltaCodec: DeltaCodec[Data] =
//    prismN(Data.postPrism) or prismN(Data.addressPrism) or prismN(Data.userPrism) or prismN(Data.tagPrism)

  // Codecs to allow use of top-level data types in mirror
  implicit val addressMirrorCodec: MirrorCodec[Address] = MirrorCodec[Address]("address")
  implicit val tagMirrorCodec: MirrorCodec[Tag] = MirrorCodec[Tag]("tag")
  implicit val userMirrorCodec: MirrorCodec[User] = MirrorCodec[User]("user")
  implicit val postMirrorCodec: MirrorCodec[Post] = MirrorCodec[Post]("post")

  // Encoder and decoder for entire Mirror
  implicit val mirrorDecoder: Decoder[Mirror] = Mirror.decoder(addressMirrorCodec, tagMirrorCodec, userMirrorCodec, postMirrorCodec)
  implicit val mirrorEncoder: Encoder[Mirror] = Mirror.encoder

  val examplePostIO: DeltaIO[(Post, Mirror)] = for {
    addressId <- getId[Address]
    userId <- getId[User]
    postId <- getId[Post]
    tagId1 <- getId[Tag]
    tagId2 <- getId[Tag]
    m1 = Mirror.empty
    m2 <- m1.updated(Address(addressId, "Street Name", 42))
    m3 <- m2.updated(User(userId, "User", Ref(addressId)))
    m4 <- m3.updated(Post(
      id = postId,
      message = "Hello World!",
      userRef = Ref(userId),
      tagRefs = List(Ref(tagId1), Ref(tagId2)))
    )
    m5 <- m4.updated(Tag(tagId1, "Tag1"))
    m6 <- m5.updated(Tag(tagId2, "Tag2"))
  } yield {
    (
      //FIXME get
      m6.get(postId).get,
      m6
    )
  }

  val examplePost: (Post, Mirror) = DeltaIORun.runDeltaIO(examplePostIO, DeltaIOContext(Moment(100)), DeltaId(ClientId(1), ClientDeltaId(1)))

  def checkRefsGraph(post: Post, mirror: Mirror): Unit = {
    assert(mirror.incomingRefs(post.id) == Set.empty)
    assert(mirror.outgoingRefs(post.id) == Set(post.userRef.guid) ++ post.tagRefs.map(_.guid))

    val r = for {
      user <- mirror(post.userRef)
      address <- mirror(user.addressRef)
      tag1 <- mirror(post.tagRefs.head)
      tag2 <- mirror(post.tagRefs(1))
    } yield {
      assert(mirror.incomingRefs(user.id) == Set(post.id))
      assert(mirror.outgoingRefs(user.id) == Set(address.id))

      assert(mirror.incomingRefs(address.id) == Set(user.id))
      assert(mirror.outgoingRefs(address.id) == Set())

      assert(mirror.incomingRefs(tag1.id) == Set(post.id))
      assert(mirror.outgoingRefs(tag1.id) == Set())

      assert(mirror.incomingRefs(tag2.id) == Set(post.id))
      assert(mirror.outgoingRefs(tag2.id) == Set())
      true
    }

    assert(r.isDefined)
  }

  def checkMirrorContents(post: Post, mirror: Mirror): Unit = {
    assert(mirror.incomingRefs(post.id) == Set.empty)
    assert(mirror.outgoingRefs(post.id) == Set(post.userRef.guid) ++ post.tagRefs.map(_.guid))

    val mo = examplePost._2

    val r = for {
      postAgain <- mirror.get(post.id)
      userAgain <- mirror(postAgain.userRef)
      addressAgain <- mirror(userAgain.addressRef)
      tag1Again <- mirror(postAgain.tagRefs.head)
      tag2Again <- mirror(postAgain.tagRefs(1))

      postAgainR <- mirror.revisionOf(post.id)
      userAgainR <- mirror.revisionOf(postAgain.userRef)
      addressAgainR <- mirror.revisionOf(userAgain.addressRef)
      tag1AgainR <- mirror.revisionOf(postAgain.tagRefs.head)
      tag2AgainR <- mirror.revisionOf(postAgain.tagRefs(1))

      user <- mo(post.userRef)
      address <- mo(user.addressRef)
      tag1 <- mo(post.tagRefs.head)
      tag2 <- mo(post.tagRefs(1))

      postR <- mo.revisionOf(post.id)
      userR <- mo.revisionOf(post.userRef)
      addressR <- mo.revisionOf(user.addressRef)
      tag1R <- mo.revisionOf(post.tagRefs.head)
      tag2R <- mo.revisionOf(post.tagRefs(1))

    } yield {
      assert(postAgain === post)
      assert(userAgain === user)
      assert(addressAgain === address)
      assert(tag1Again === tag1)
      assert(tag2Again === tag2)

      assert(postAgainR === postR)
      assert(userAgainR === userR)
      assert(addressAgainR === addressR)
      assert(tag1AgainR === tag1R)
      assert(tag2AgainR === tag2R)

      true
    }

    assert(r.isDefined)
  }

  "Mirror" should {
    "produce correct refs graph" in {
      val mirror = examplePost._2
      val post = examplePost._1
      checkRefsGraph(post, mirror)
    }
  }

  "MirrorCodec" should {
    "encode, decode and re-encode mirror to same Json" in {
      val encoded = mirrorEncoder(examplePost._2)
      mirrorDecoder.decodeJson(encoded) match {
        case Left(error) => fail(error)
        case Right(md) =>
          val reEncode = mirrorEncoder(md)
          assert(encoded === reEncode)
      }
    }

    "encode and decode to same contents and revisions" in {
      val post = examplePost._1
      val encoded = mirrorEncoder(examplePost._2)
      mirrorDecoder.decodeJson(encoded) match {
        case Left(error) => fail(error)
        case Right(md) =>
          checkRefsGraph(post, md)
          checkMirrorContents(post, md)
      }
    }

  }

  "DeltaCodecs" should {
    "update refs" in {
      pending
//      val post = examplePost._1
//      val postUpdated = postDeltaDecoder.updateRefsDataOnly(examplePost._1, examplePost._2)
//
//      //All refs should be updated to rev 0
//      val postExpected = Post(
//        id = post.id,
//        message = post.message,
//        userRef = RefResolved(post.userRef.guid, 0),
//        tagRefs = List(
//          RefResolved(post.tagRefs.head.guid, 0),
//          RefResolved(post.tagRefs(1).guid, 0)
//        )
//      )
//
//      assert(postUpdated == postExpected)
    }

    "find outgoing refs" in {
      val post = examplePost._1
      val rur = postDeltaDecoder.updateRefs(RefUpdateResult.noRefs(post), examplePost._2)

      assert(rur.outgoingRefs == Set(post.userRef.guid) ++ post.tagRefs.map(_.guid))
    }

    "update refs incrementally" in {
      pending
//      val post = examplePost._1
//      val cache = examplePost._2
//
//      //Update the post's refs so they have revisions
//      val postUpdated = postDeltaDecoder.updateRefsDataOnly(examplePost._1, examplePost._2)
//
//      // Get the user from the cache using updated ref having revision
//      val user = cache.prism(postUpdated.userRef).get
//
//      // Update the user data, and update the cache with thi data
//      val userUpdated = user.copy(name = "UserModified")
//      val cacheUpdated = cache.updated(userUpdated)
//
//      //Now update the post's refs again with the updated cache
//      val postUpdated2 = postDeltaDecoder.updateRefsDataOnly(postUpdated, cacheUpdated)
//
//      //User ref should be updated to rev 1, tags still at 0
//      val postExpected = Post(
//        id = post.id,
//        message = post.message,
//        userRef = RefResolved(post.userRef.guid, 1),
//        tagRefs = List(
//          RefResolved(post.tagRefs(0).guid, 0),
//          RefResolved(post.tagRefs(1).guid, 0)
//        )
//      )
//      assert(postUpdated2 == postExpected)
//
//      // And we can still get at the user in the updated cache, even with older refs
//      assert(cacheUpdated(post.userRef).contains(userUpdated))
//      assert(cacheUpdated(postUpdated.userRef).contains(userUpdated))
//      assert(cacheUpdated(postUpdated2.userRef).contains(userUpdated))
//
    }

  }

}
