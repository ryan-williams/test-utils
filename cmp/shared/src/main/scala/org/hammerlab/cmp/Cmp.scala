package org.hammerlab.cmp

import hammerlab.option._

object Cmp {
  type Aux[T, Δ] = CanEq.Aux[T, T, Δ]

  /** Create a [[Cmp]] from its single method */
  def apply[T, Δ](fn: (T, T) ⇒ ?[Δ]): Aux[T, Δ] = CanEq(fn)

  def ???[T, Δ] = Cmp[T, Δ] { (_, _) ⇒ Predef.??? }

  /** Create a [[Cmp]] interms of another */
  def by[L, R](fn: L ⇒ R)(implicit cmp: Cmp[R]): Aux[L, cmp.Δ] =
    Cmp {
      (l, r) ⇒
        cmp(
          fn(l),
          fn(r)
        )
    }

  /**
   * Wrapper around a [[Cmp]] used to enable overloading methods that would otherwise have the same type after erasure
   */
  case class Wrapper[T, D](cmp: Cmp.Aux[T, D]) {
    type Δ = cmp.Δ
  }
  object Wrapper {
    implicit def   wrap[T, Δ](implicit c: Cmp.Aux[T, Δ]): Wrapper[T, Δ] = Wrapper(c)
    implicit def unwrap[T, Δ](         w: Wrapper[T, Δ]): Cmp.Aux[T, Δ] = w.cmp
  }
}
