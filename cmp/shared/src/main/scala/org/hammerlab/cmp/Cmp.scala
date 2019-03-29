package org.hammerlab.cmp

object Cmp {
  type Aux[T, E] = CanEq.Aux[T, T, E]

  /** Create a [[Cmp]] from its single method */
  def apply[T, E](fn: (T, T) ⇒ Option[E]): Aux[T, E] = CanEq(fn)

  /** Create a [[Cmp]] interms of another */
  def by[L, R](fn: L ⇒ R)(implicit cmp: Cmp[R]): Aux[L, cmp.Diff] =
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
    type Diff = cmp.Diff
  }
  object Wrapper {
    implicit def   wrap[T, D](implicit c: Cmp.Aux[T, D]): Wrapper[T, D] = Wrapper(c)
    implicit def unwrap[T, D](         w: Wrapper[T, D]): Cmp.Aux[T, D] = w.cmp
  }
}
