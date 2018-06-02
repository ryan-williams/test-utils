package org.hammerlab.cmp.first

import org.hammerlab.cmp.Pos
import org.hammerlab.test.Cmp
import shapeless._
import hammerlab.cmp.first.SealedTrait

trait either {
  import either.eitherCanEq
  implicit def cmpEitherProductSerializable[L, R, EL, ER](
    implicit
    cmpL: Lazy[Cmp.Aux[L, EL]],
    cmpR: Lazy[Cmp.Aux[R, ER]],
    pos: Pos
  ):
    Cmp.Aux[
      Either[L, R] with Product with Serializable,
      SealedTrait.Err[
        (EL :+: CNil) :+:
        (ER :+: CNil) :+:
        CNil
      ]
    ] = {
      println(s"**** $pos: eitherProductSerializable")
      eitherCanEq[L, R, EL, ER]
    }
}

object either {
  import cats.implicits.{ catsKernelStdOrderForInt, catsKernelStdOrderForString }

  def eitherCanEq[L, R, EL, ER](
    implicit
    cmpL: Lazy[Cmp.Aux[L, EL]],
    cmpR: Lazy[Cmp.Aux[R, ER]]
  ):
    Cmp.Aux[
      Either[L, R],
      SealedTrait.Err[
        (EL :+: CNil) :+:
        (ER :+: CNil) :+:
        CNil
      ]
    ] = implicitly
}