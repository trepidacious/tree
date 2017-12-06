package org.rebeam.tree.sync

import io.circe.{Decoder, Encoder}
import io.circe.generic.JsonCodec
import org.rebeam.lenses.macros.Lenses
import org.rebeam.tree.Delta._
import org.rebeam.tree.DeltaCodecs._
import org.rebeam.tree.sync.Sync._
import org.rebeam.tree._
import org.rebeam.tree.sync._
import org.scalatest._
import org.scalatest.prop.Checkers

import scala.language.higherKinds
import org.rebeam.tree.BasicDeltaDecoders._
import org.rebeam.tree.ref.{Mirror, MirrorCodec}

class RefSpec extends WordSpec with Matchers with Checkers {

  @JsonCodec
  sealed trait Data extends Identified[Data]

  object Data {
    @JsonCodec
    @Lenses
    case class Address(id: Id[Address], streetName: String, number: Int) extends Identified[Address] with Data

    @JsonCodec
    @Lenses
    case class User(id: Id[User], name: String, addressRef: Ref[Address]) extends Identified[User] with Data

    @JsonCodec
    @Lenses
    case class Tag(id: Id[Tag], tag: String) extends Identified[Tag] with Data

    @JsonCodec
    @Lenses
    case class Post(id: Id[Post], message: String, userRef: Ref[User], tagRefs: List[Ref[Tag]]) extends Identified[Post] with Data

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

  val examplePost: (Post, Mirror) =
    DeltaIORun.runDeltaIO(
      examplePostIO,
      DeltaIOContext(Moment(100)), DeltaId(ClientId(1), ClientDeltaId(1))
    ).data

  def checkMirrorContents(post: Post, mirror: Mirror): Unit = {
    val mo = examplePost._2

    val r = for {
      postAgain <- mirror.get(post.id)
      userAgain <- mirror(postAgain.userRef)
      addressAgain <- mirror(userAgain.addressRef)
      tag1Again <- mirror(postAgain.tagRefs.head)
      tag2Again <- mirror(postAgain.tagRefs(1))

      postAgainR <- mirror.revisionOf(post.id.guid)
      userAgainR <- mirror.revisionOf(postAgain.userRef)
      addressAgainR <- mirror.revisionOf(userAgain.addressRef)
      tag1AgainR <- mirror.revisionOf(postAgain.tagRefs.head)
      tag2AgainR <- mirror.revisionOf(postAgain.tagRefs(1))

      user <- mo(post.userRef)
      address <- mo(user.addressRef)
      tag1 <- mo(post.tagRefs.head)
      tag2 <- mo(post.tagRefs(1))

      postR <- mo.revisionOf(post.id.guid)
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
          checkMirrorContents(post, md)
      }
    }

  }

}
