package org.hammerlab.cmp.first

import org.hammerlab.test.Cmp
import shapeless._

trait CaseClass
  extends either {
  implicit def cmpCaseClass[T, L <: HList, E <: Coproduct](
    implicit
    gen: Generic.Aux[T, L],
    listCmp: Lazy[Cmp.Aux[L, E]]
  ):
    Cmp.Aux[T, E] = {
    println(s"cmpCaseClass called: ${listCmp.value}")
    val cmp =
    Cmp[T, E] {
      (l, r) ⇒
        listCmp
          .value
          .cmp(
            gen.to(l),
            gen.to(r)
          )
    }
    println(s"cmpCaseClass returning: $cmp from ${listCmp.value}")
    cmp
  }

  implicit def cmpCons[H, T <: HList, ET <: Coproduct](
    implicit
    head: Lazy[Cmp[H]],
    tail: Lazy[Cmp.Aux[T, ET]]
  ):
    Cmp.Aux[H :: T, head.value.Error :+: ET] = {
    println(s"cmpCons called: ${head.value} ${tail.value}")
    val ret =
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
    println(s"cmpCons returning: $ret from ${head.value} ${tail.value}")
    ret
  }

  implicit val cmpHNil: Cmp.Aux[HNil, CNil] = Cmp[HNil, CNil]((_, _) ⇒ None)
  println(s"cmpHNil: $cmpHNil")
}
