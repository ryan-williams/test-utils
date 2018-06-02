package org.hammerlab

import cats.Eq
import hammerlab.math.tolerance._
import org.hammerlab.cmp.CanEq.withConversion
import org.hammerlab.cmp.{ CanEq, double, dsl }
import org.hammerlab.test.{ Afters, Befores, matchers }
import org.scalatest.{ FunSuite, Matchers }

/**
 * Base for scalatest [[FunSuite]] with many extensions related to setup/teardown, custom equality-testing, etc.
 *
 * The `==` method asserts equality between two objects, but allows for configuring the returned expression of what
 * differed between them (via a [[CanEq]] instance), and how that structured-diff should be displayed (via a
 * [[cats.Show]]).
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

  /** Fuzziness for [[Double]] assertions / equality-comparisons; see [[fuzzyDoubleCmp]] */
  implicit var ε: E = 1e-6

  /** Convenience-setter for [[ε]] */
  def tolerance(d: Double): Unit = {
    ε = d
  }

  /** Default [[Eq]] instances that [[org.hammerlab.test.Cmp]]s can be derived from */
  implicit val     intOrder: Eq[    Int] = cats.instances.    int.catsKernelStdOrderForInt
  implicit val    longOrder: Eq[   Long] = cats.instances.   long.catsKernelStdOrderForLong
  implicit val  stringOrder: Eq[ String] = cats.instances. string.catsKernelStdOrderForString
  implicit val booleanOrder: Eq[Boolean] = cats.instances.boolean.catsKernelStdOrderForBoolean

  /** Default [[CanEq]] instances for [[Double]]s vs. [[Int]]s */
  implicit val int2double : CanEq[Double, Int] = withConversion[Double, Int]
  implicit val int2long   : CanEq[  Long, Int] = withConversion[  Long, Int]
}


