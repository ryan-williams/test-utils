package org.hammerlab

import org.hammerlab.cmp.CanEq.withConversion
import org.hammerlab.cmp.{ CanEq, double }
import org.hammerlab.math.syntax.E
import org.hammerlab.test.matchers.lines.HasMatcher
import org.hammerlab.test.{ Afters, Befores }
import org.scalatest.{ FunSuite, Matchers }

abstract class Suite
  extends FunSuite
    with Matchers
    with Befores
    with Afters
    with HasMatcher
    with double.HasNeq {

  /** Fuzziness for [[Double]] assertions / equality-comparisons; see [[doubleCmp]] */
  implicit var ε: E = 1e-6

  /** Convenience-setter for [[ε]] */
  def tolerance(d: Double): Unit = {
    ε = d
  }

  implicit val  intOrder = cats.instances. int.catsKernelStdOrderForInt
  implicit val longOrder = cats.instances.long.catsKernelStdOrderForLong

  /** Default [[CanEq]] instances for [[Double]]s vs. [[Int]]s */
  implicit val int2double : CanEq[Double, Int] = withConversion[Double, Int]
  implicit val int2long   : CanEq[  Long, Int] = withConversion[  Long, Int]

  def ===[T, U](t1: T, t2: U)(implicit canEqual: CanEq[T, U]): Unit =
    canEqual
      .cmp(t1, t2)
      .foreach {
        e ⇒
          fail(
            e.toString
          )
      }

  def !==[T, U](t1: T, t2: U)(implicit canEqual: CanEq[T, U]): Unit =
    if (canEqual.cmp(t1, t2).isEmpty)
      fail(s"Expected $t1 !== $t2")
}
