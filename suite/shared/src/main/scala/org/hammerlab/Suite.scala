package org.hammerlab

import cats.Eq
import org.hammerlab.math.syntax.FuzzyCmp.FuzzyCmpOps
import org.hammerlab.math.syntax.Tolerance
import org.hammerlab.test.CanEq.withConversion
import org.hammerlab.test.{ Afters, Befores, CanEq, MkEqDerivation }
import org.scalatest.{ FunSuite, Matchers }

import scala.math.abs

abstract class Suite
  extends FunSuite
    with Matchers
    with Befores
    with Afters
    with MkEqDerivation {

  implicit val  intOrder = cats.instances. int.catsKernelStdOrderForInt
  implicit val longOrder = cats.instances.long.catsKernelStdOrderForLong

  /** Fuzziness for [[Double]] assertions / equality-comparisons; see [[doubleEq]] */
  implicit var ε: Tolerance = 1e-6

  /** Convenience-setter for [[ε]] */
  def tolerance(d: Double): Unit = {
    ε = d
  }

  /** Default [[CanEq]] instances for [[Double]]s vs. [[Int]]s */
  implicit val int2double : CanEq[Double, Int] = withConversion[Double, Int]
  implicit val int2long   : CanEq[  Long, Int] = withConversion[  Long, Int]

  /**
   * Consider [[Double]]s to be equal if their ratio if their ratio differs from [[1]] by less than or equal to [[ε]],
   * or if one of them equals zero and the other's absolute value is less than or equal to [[ε]]
   */
  implicit def doubleEq: Eq[Double] =
    new Eq[Double] {
      override def eqv(x: Double, y: Double) =
        if (x == 0 || y == 0)
          abs(x + y) + 1 <= ε
        else
          new FuzzyCmpOps(x).===(y)
    }

  def ===[T, U](t1: T, t2: U)(implicit canEqual: CanEq[T, U]): Unit = {
    if (!canEqual.eqv(t1, t2))
      fail(s"$t1 didn't match $t2")
  }
}
