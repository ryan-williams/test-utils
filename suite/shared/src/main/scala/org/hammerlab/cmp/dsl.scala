package org.hammerlab.cmp

import org.scalatest.FunSuite

trait dsl {
  self: FunSuite ⇒

  /**
   * Assert equality between two arbitrary types
   *
   * Supports implicit configuration of:
   *
   * - equality-test logic ([[CanEq]])
   * - error / "diff" type ([[D]])
   * - display of error/diff (via [[cats.Show]] instances)
   *
   * @param l left-hand comparee
   * @param r right-hand comparee
   * @param cmp comparator; returns arbitrary error-type [[D]]
   * @param showError customizable pretty-printer for error-type [[D]]
   * @tparam L left-hand comparee type
   * @tparam R right-hand comparee type
   * @tparam D error / "diff" type; a given [[CanEq]] can expose different expressions of the computed difference
   *           between its compared instances
   */
  def ===[L, R, D](
    l: L,
    r: R
  )(
    implicit
    cmp: CanEq.Aux[L, R, D],
    showError: Show[D]
  ): Unit =
    cmp(l, r)
      .foreach {
        e ⇒
          fail(
            showError(e)
          )
      }

  /**
   * Verify that two objects are not equal, according to an implicit [[CanEq]]
   */
  def !==[L, R](l: L, r: R)(implicit cmp: CanEq[L, R]): Unit =
    if (cmp(l, r).isEmpty)
      fail(s"Expected $l !== $r")

  def !=[L, R](l: L, r: R)(implicit cmp: CanEq[L, R]): Unit =
    if (cmp(l, r).isEmpty)
      fail(s"Expected $l !== $r")

  def ==[L, R, D](
    l: L,
    r: R
  )(
    implicit
    cmp: CanEq.Aux[L, R, D],
    showError: Show[D]
  ): Unit =
    ===(l, r)

  def ===[T, D](
    l: T
  )(
    r: T
  )(
    implicit
    cmp: CanEq.Wrapper[T, T, D],
    showError: Show[D]
  ): Unit =
    ===(l, r)(cmp, showError)

  def ==[T, D](
    l: T
  )(
    r: T
  )(
    implicit
    cmp: CanEq.Wrapper[T, T, D],
    showError: Show[D]
  ): Unit =
    ===[T, D](l)(r)(cmp, showError)

  def !=[T](
    l: T
  )(
    r: T
  )(
    implicit
    cmp: Cmp.Wrapper[T, _]
  ): Unit =
    if (cmp(l, r).isEmpty)
      fail(s"Expected $l !== $r")
}
