package org.hammerlab.cmp.first

import org.hammerlab.test.Cmp
import shapeless.{ :+:, CNil, Coproduct, Generic, Inl, Inr, Lazy }

trait SealedTrait {
  object SealedTrait {
    type Err[E <: Coproduct] = Either[String, E]
  }
  import SealedTrait.Err

  implicit val cmpCnil: Cmp.Aux[CNil, Err[CNil]] = Cmp[CNil, Err[CNil]]((_, _) ⇒ ???)

  implicit def cmpCCons[H, T <: Coproduct, ET <: Coproduct](
    implicit
    head: Lazy[Cmp[H]],
    tail: Lazy[Cmp.Aux[T, Err[ET]]]
  ):
    Cmp.Aux[
      H :+: T,
      Err[head.value.Error :+: ET]
    ] =
    Cmp[
      H :+: T,
      Err[head.value.Error :+: ET]
    ] {
      case (Inl(l), Inl(r)) ⇒
        head
          .value
          .cmp(l, r)
          .map(e ⇒ Right(Inl(e)))
      case (Inr(l), Inr(r)) ⇒
        tail
          .value
          .cmp(l, r)
          .map {
            case  Left(e) ⇒ Left(e)
            case Right(e) ⇒ Right(Inr(e))
          }
      case (l, r) ⇒
        def str[C <: Coproduct](c: C): String =
          c match {
            case Inl(v) ⇒ v.toString
            case Inr(v) ⇒ str(v)
            case _: CNil ⇒ ???
          }
        Some(
          Left(
            s"Different types: ${str(l)}, ${str(r)}"
          )
        )
    }

  implicit def cmpSealedTrait[T, C <: Coproduct, E <: Coproduct](
    implicit
    gen: Generic.Aux[T, C],
    cmp: Lazy[Cmp.Aux[C, Err[E]]]
  ):
    Cmp.Aux[T, Err[E]] =
    Cmp[T, Err[E]](
      (t, u) ⇒
        cmp
          .value
          .cmp(
            gen.to(t),
            gen.to(u)
          )
    )
}
