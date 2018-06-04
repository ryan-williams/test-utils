package org.hammerlab

import hammerlab.math.tolerance._
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
}
