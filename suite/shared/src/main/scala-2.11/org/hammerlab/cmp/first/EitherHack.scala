package org.hammerlab.cmp.first

import org.hammerlab.cmp.CanEq.SealedTrait.Err
import org.hammerlab.test.Cmp
import shapeless._

/**
 * Scala-2.11-only mix-in to account for [[Either]] not extending [[Product with Serializable]], cf.
 * [[https://github.com/scala/bug/issues/9173 scala/bug#9173]]
 *
 * It provides a [[Cmp]] instance for [[Either with Product with Serializable]], which is necessary for test-calls like:
 *
 * {{{
 * val l =  Left("abc")
 * val r = Right( 123 )
 *
 * // LUB is Either[String, Int] with Product with Serializable, which doesn't get auto-derived (should it get the
 * // contravariant [[Either]] instance? That doesn't happen in Scala 2.11 or 2.12, I don't think)
 * cmp(l, r)
 * }}}
 */
trait EitherHack {
  import EitherHack.eitherCanEq
  implicit def cmpEitherProductSerializable[L, R, EL, ER](
    implicit
    cmpL: Lazy[Cmp.Aux[L, EL]],
    cmpR: Lazy[Cmp.Aux[R, ER]]
  ):
    Cmp.Aux[
      Either[L, R] with Product with Serializable,
      Err[
        (EL :+: CNil) :+:
        (ER :+: CNil) :+:
        CNil
      ]
    ] =
      eitherCanEq[L, R, EL, ER]
}

object EitherHack {
  /** Bring in [[Cmp]] instances for [[Int]], [[String]] (via [[org.hammerlab.cmp.CanEq.fromEq]]) */
  import cats.implicits.{ catsKernelStdOrderForInt, catsKernelStdOrderForString }

  /**
   * "Clean" derivation of a [[Cmp]] for [[Either]] that will in turn derive instances for [[Left]] and [[Right]] (for
   * use by `cmpEitherProductSerializable` above) without recursively using `cmpEitherProductSerializable` (which it
   * would do if `cmpEitherProductSerializable` were in scope, since [[Left]] and [[Right]] are each sub-types of
   * [[Either with Product with Serializable]].
   */
  def eitherCanEq[L, R, EL, ER](
    implicit
    cmpL: Lazy[Cmp.Aux[L, EL]],
    cmpR: Lazy[Cmp.Aux[R, ER]]
  ):
    Cmp.Aux[
      Either[L, R],
      Err[
        (EL :+: CNil) :+:
        (ER :+: CNil) :+:
        CNil
      ]
    ] = implicitly
}
