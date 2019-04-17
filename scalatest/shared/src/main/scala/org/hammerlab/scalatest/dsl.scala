package org.hammerlab.scalatest

import org.hammerlab.cmp.{ CanEq, Cmp, Show }
import org.scalatest.FunSuite
import org.scalatest.exceptions.TestFailedException

trait dsl {
  self: FunSuite ⇒

  @inline def `☠️`[Δ](Δ: Δ)(implicit show: Show[Δ]) = {
    //throw new TestFailedException(Some(show(Δ)), Some(new Exception), 4)
    fail(show(Δ), new Exception)
  }

  /**
   * Assert equality between two arbitrary types
   *
   * Supports implicit configuration of:
   *
   * - equality-test logic ([[CanEq]])
   * - error / "diff" type ([[Δ]])
   * - display of error/diff (via [[cats.Show]] instances)
   *
   * @param l left-hand comparee
   * @param r right-hand comparee
   * @param cmp comparator; returns arbitrary error-type [[Δ]]
   * @param showError customizable pretty-printer for error-type [[Δ]]
   * @tparam L left-hand comparee type
   * @tparam R right-hand comparee type
   * @tparam Δ error / "diff" type; a given [[CanEq]] can expose different expressions of the computed difference
   *           between its compared instances
   */
  def ===[L, R, Δ](
    l: L,
    r: R
  )(
    implicit
    cmp: CanEq.Aux[L, R, Δ],
    showError: Show[Δ]
  ): Unit =
    cmp(l, r)
      .foreach {
        Δ ⇒
          `☠️`(
          //throw new Exception(
            showError(Δ),
            new Exception
          )
      }

  /**
   * Verify that two objects are not equal, according to an implicit [[CanEq]]
   */
  def !==[L, R](l: L, r: R)(implicit cmp: CanEq[L, R]): Unit =
    if (cmp(l, r).isEmpty)
      `☠️`(s"Expected $l !== $r")

  def !=[L, R](l: L, r: R)(implicit cmp: CanEq[L, R]): Unit =
    if (cmp(l, r).isEmpty)
      `☠️`(s"Expected $l !== $r")

  def ==[L, R, Δ](
    l: L,
    r: R
  )(
    implicit
    cmp: CanEq.Aux[L, R, Δ],
    showError: Show[Δ]
  ): Unit =
    ===(l, r)

  def ===[T, Δ](
    l: T
  )(
    r: T
  )(
    implicit
    cmp: CanEq.Wrapper[T, T, Δ],
    showError: Show[Δ]
  ): Unit =
    ===(l, r)(cmp, showError)

  def ==[T, Δ](
    l: T
  )(
    r: T
  )(
    implicit
    cmp: CanEq.Wrapper[T, T, Δ],
    showError: Show[Δ]
  ): Unit =
    ===[T, Δ](l)(r)(cmp, showError)

  def !=[T](
    l: T
  )(
    r: T
  )(
    implicit
    cmp: Cmp.Wrapper[T, _]
  ): Unit =
    if (cmp(l, r).isEmpty)
      `☠️`(s"Expected $l !== $r")
}
