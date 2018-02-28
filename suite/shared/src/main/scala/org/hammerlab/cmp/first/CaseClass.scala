package org.hammerlab.cmp.first

import org.hammerlab.test.Cmp
import shapeless._

trait CaseClass {
  implicit def cclass[T, L <: HList, E <: Coproduct](implicit
                                                     gen: Generic.Aux[T, L],
                                                     listEq: Lazy[Cmp.Aux[L, E]]): Cmp[T] =
    Cmp[T, E] {
      (l, r) ⇒
        listEq
          .value
          .cmp(
            gen.to(l),
            gen.to(r)
          )
    }

  implicit def cons[H, T <: HList, ET <: Coproduct](implicit
                                                    head: Lazy[Cmp[H]],
                                                    tail: Lazy[Cmp.Aux[T, ET]]): Cmp.Aux[H :: T, head.value.Error :+: ET] =
    Cmp[H :: T, head.value.Error :+: ET](
      (l, r) ⇒
        head
          .value
          .cmp(l.head, r.head)
          .map(Inl[head.value.Error, ET](_))
          .orElse(
            tail
              .value
              .cmp(l.tail, r.tail)
              .map(Inr[head.value.Error, ET](_))
          )
    )

  implicit val hnil: Cmp.Aux[HNil, CNil] = Cmp[HNil, CNil]((_, _) ⇒ None)
}
