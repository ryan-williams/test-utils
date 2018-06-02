package org.hammerlab

import cats.Eq
import hammerlab.math.tolerance._
import org.hammerlab.cmp.CanEq.withConversion
import org.hammerlab.cmp.{ CanEq, Wrapper, double }
import org.hammerlab.test.{ Afters, Befores, matchers }
import org.scalatest.{ FunSuite, Matchers }

/**
 * Base for scalatest [[FunSuite]] with many extensions related to setup/teardown, custom equality-testing, etc.
 *
 * The `===` method asserts equality between two objects, but allows for configuring the returned expression of what
 * differed between them (via a [[CanEq]] instance), and how that structured-diff should be displayed (via a [[Show]]).
 *
 * For example, tests can import implicit [[CanEq]]s to compare collections of objects in a variety of ways:
 *
 * - configurable "fuzziness" to [[Double]]-equality (see `Suite.ε` below)
 * - returning a representation of collections' difference in terms of:
 *   - the index of the first elements to differ (see [[org.hammerlab.cmp.first.Collections]])
 *   - more complex logic to e.g. express that collections have the same elements, but in a different order (TODO:
 *     implement this, and others like it!)
 */
abstract class Suite
  extends FunSuite
     with Matchers
     with Befores
     with Afters
     with matchers.lines.HasMatcher
     with matchers.seqs.all
     with double.HasNeq
     with CanEq.dsl
     with dsl {

  /** Fuzziness for [[Double]] assertions / equality-comparisons; see [[doubleCmp]] */
  implicit var ε: E = 1e-6

  /** Convenience-setter for [[ε]] */
  def tolerance(d: Double): Unit = {
    ε = d
  }

  /** Default [[Eq]] instances that [[org.hammerlab.test.Cmp]]s can be derived from */
  implicit val    intOrder: Eq[   Int] = cats.instances.   int.catsKernelStdOrderForInt
  implicit val   longOrder: Eq[  Long] = cats.instances.  long.catsKernelStdOrderForLong
  implicit val stringOrder: Eq[String] = cats.instances.string.catsKernelStdOrderForString

  /** Default [[CanEq]] instances for [[Double]]s vs. [[Int]]s */
  implicit val int2double : CanEq[Double, Int] = withConversion[Double, Int]
  implicit val int2long   : CanEq[  Long, Int] = withConversion[  Long, Int]

}

trait Show[T] {
  def apply(t: T): String
}
trait LowPriorityShow
  extends Serializable {
  /** Fallback [[Show]] for error-types that don't have a custom [[Show]] provided */
  implicit def showAny[T]: Show[T] = Show { t: T ⇒ t.toString }
}
object Show
  extends LowPriorityShow {
  implicit def fromCats[T](implicit s: cats.Show[T]): Show[T] = Show { s.show }
  def apply[T](fn: T ⇒ String): Show[T] =
    new Show[T] {
      def apply(t: T): String = fn(t)
    }
}

trait dsl {
  self: FunSuite ⇒

  /**
   * Assert equality between two arbitrary types
   *
   * Supports implicit configuration of:
   *
   * - equality test ([[CanEq]])
   * - error / "diff" type ([[E]])
   * - display of error/diff ([[Show]])
   *
   * @param l left-hand comparee
   * @param r right-hand comparee
   * @param cmp comparator; returns arbitrary error-type [[E]]
   * @param showError customizable pretty-printer for error-type [[E]]
   * @tparam L left-hand comparee type
   * @tparam R right-hand comparee type
   * @tparam E error / "diff" type; a given [[CanEq]] can expose different expressions of the computed difference
   *           between its compared instances
   */
  def ===[L, R, E](
    l: L,
    r: R
  )(
    implicit
    cmp: CanEq.Aux[L, R, E],
    showError: Show[E]
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

  def ==[L, R, E](
    l: L,
    r: R
  )(
    implicit
    cmp: CanEq.Aux[L, R, E],
    showError: Show[E]
  ): Unit =
    cmp(l, r)
      .foreach {
        e ⇒
          fail(
            showError(e)
          )
      }

  def ===[T, E](
    l: T
  )(
    r: T
  )(
    implicit
    cmp: Wrapper[T, T, E],
    showError: Show[E]
  ): Unit =
    cmp(l, r)
      .foreach {
        e ⇒
          fail(
            showError(e)
          )
      }

  def ==[T, E](
    l: T
  )(
    r: T
  )(
    implicit
    cmp: Wrapper[T, T, E],
    showError: Show[E]
  ): Unit =
    cmp(l, r)
      .foreach {
        e ⇒
          fail(
            showError(e)
          )
      }

  def !=[T](
    l: T
  )(
    r: T
  )(
    implicit
    cmp: Wrapper[T, T, _]
  ): Unit =
    if (cmp(l, r).isEmpty)
      fail(s"Expected $l !== $r")
}
