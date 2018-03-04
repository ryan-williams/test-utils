package org.hammerlab.cmp.first

import org.hammerlab.test.Cmp
import shapeless.{ :+:, CNil, Coproduct, Generic, Inl, Inr, Lazy }

trait SealedTrait {
  implicit val cnil: Cmp.Aux[CNil, CNil] = Cmp[CNil, CNil]((_, _) ⇒ ???)

  implicit def ccons[H, T <: Coproduct, ET <: Coproduct](implicit
                                                         head: Lazy[Cmp[H]],
                                                         tail: Lazy[Cmp.Aux[T, ET]]): Cmp.Aux[H :+: T, Either[head.value.Error, String] :+: ET] =
    Cmp[H :+: T, Either[head.value.Error, String] :+: ET] {
      case (Inl(l), Inl(r)) ⇒
        head
          .value
          .cmp(l, r)
          .map(e ⇒ Inl(Left(e)))
      case (Inr(l), Inr(r)) ⇒
        tail
          .value
          .cmp(l, r)
          .map(Inr(_))
      case (l, r) ⇒
        Some(
          Inl(
            Right(
                s"Different types: $l $r"
            )
          )
        )
    }

  implicit def sealedTrait[T, C <: Coproduct, E <: Coproduct](implicit
                                                              gen: Generic.Aux[T, C],
                                                              cmp: Lazy[Cmp.Aux[C, E]]): Cmp[T] =
    Cmp[T, E](
      (t, u) ⇒
        cmp
          .value
          .cmp(
            gen.to(t),
            gen.to(u)
          )
    )

}
