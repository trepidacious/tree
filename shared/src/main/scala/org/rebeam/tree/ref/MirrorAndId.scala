package org.rebeam.tree.ref

import org.rebeam.lenses.macros.Lenses
import org.rebeam.tree.sync.Sync._
import org.rebeam.tree.DeltaCodecs._
import org.rebeam.tree.Delta._
import scala.language.higherKinds
import io.circe._, io.circe.generic.semiauto._

@Lenses
case class MirrorAndId[M](mirror: Mirror, id: Guid[M])

object MirrorAndId {
  import Guid._
  // Note There is no general Mirror codec (depends on the types in the mirror), so we need to have typeclasses for Mirror as implicit parameters, not a static import

  // Can only edit mirror in MirrorAndId - the id stays the same.
  implicit def mirrorAndIdDeltaCodec[M](implicit mirrorDeltaCodec: DeltaCodec[Mirror], mDeltaCodec: DeltaCodec[M]): DeltaCodec[MirrorAndId[M]] = lensN(MirrorAndId.mirror)
  implicit def mirrorAndIdDecoder[M](implicit mirrorDecoder: Decoder[Mirror]): Decoder[MirrorAndId[M]] = deriveDecoder[MirrorAndId[M]]
  implicit def mirrorAndIdEncoder[M](implicit mirrorEncoder: Encoder[Mirror]): Encoder[MirrorAndId[M]] = deriveEncoder[MirrorAndId[M]]

  def apply[M: ToId](d: DeltaIO[M]): DeltaIO[MirrorAndId[M]] = for {
    m <- d
  } yield MirrorAndId(Mirror.empty, implicitly[ToId[M]].id(m))
}