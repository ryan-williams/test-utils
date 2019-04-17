package org.hammerlab.cmp

import shapeless.{ =:!=, Lazy }

trait ConvertLHS
  extends Serializable
{

  /**
   * Derive a [[CanEq]] for two types given a [[Cmp]] for the second and an implicit conversion from the first to the
   * second
   */
  implicit def convertActualCanEq[
    L,
    R
  ](
    implicit
    cmp: Lazy[Cmp[R]],
    evl: L =:!= R,
    fn: L ⇒ R
  ):
    CanEq.Aux[
      L,
      R,
      cmp.value.Δ
    ] =
    CanEq {
      (l, r) ⇒
        cmp.value(l, r)
    }

  /**
   * Derive a [[Cmp]] from an [[Ordering]]; lower priority than [[CanEq.fromEq]] (derivation from [[cats.Eq]])
   */
  implicit def cmpFromOrdering[T](implicit ord: Ordering[T]): Cmp.Aux[T, (T, T)] =
    Cmp {
      (l, r) ⇒
        if (ord.equiv(l, r))
          None
        else
          Some((l, r))
    }
}
