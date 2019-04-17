package org.hammerlab.cmp

import shapeless.{ =:!=, Lazy }

trait ConvertRHS
  extends ConvertLHS {
  /**
   * Derive a [[CanEq]] for two types given a [[Cmp]] for the first and an implicit conversion from the second to the
   * first
   */
  implicit def convertExpectedCanEq[
    L,
    R
  ](
    implicit
    cmp: Lazy[Cmp[L]],
    evl: L =:!= R,
    fn: R ⇒ L
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
}
