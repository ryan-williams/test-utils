package org.hammerlab.cmp

import shapeless.{ =:!=, Lazy }

trait Casts
  extends ConvertRHS {

  /**
   * Derive a [[CanEq]] for two types where the second is a ([[=:!= strict!]]) subclass of the first
   */
  implicit def widenRight[
    L,
    R
  ](
    implicit
    cmp: Lazy[Cmp[L]],
    ev: L =:!= R,
    ev2: R <:< L,
  ):
    CanEq.Aux[
      L,
      R,
      cmp.value.Diff
    ] =
    CanEq {
      (l, r) ⇒
        cmp.value(l, r)
    }

  /**
   * Derive a [[CanEq]] for two types where the first is a ([[=:!= strict!]]) subclass of the second
   */
  implicit def widenLeft[
    L,
    R
  ](
    implicit
    cmp: Lazy[Cmp[R]],
    ev: L =:!= R,
    ev2: L <:< R
  ):
    CanEq.Aux[
      L,
      R,
      cmp.value.Diff
    ] =
    CanEq {
      (l, r) ⇒
        cmp.value(l, r)
    }
}
