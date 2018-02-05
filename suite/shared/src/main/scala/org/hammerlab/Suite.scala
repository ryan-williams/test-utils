package org.hammerlab

import org.hammerlab.math.syntax.FuzzyCmp.FuzzyCmpOps
import org.hammerlab.math.syntax.E
import org.hammerlab.test.CanEq.withConversion
import org.hammerlab.test.{ Afters, Befores, CanEq, Cmp, MkEqDerivation }
import org.scalatest.{ FunSuite, Matchers }

import scala.math.abs

abstract class Suite
  extends FunSuite
    with Matchers
    with Befores
    with Afters
    with MkEqDerivation {

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

  /**
   * Consider [[Double]]s to be equal if their ratio if their ratio differs from [[1]] by less than or equal to [[ε]],
   * or if one of them equals zero and the other's absolute value is less than or equal to [[ε]]
   */
  implicit def doubleCmp(implicit ε: E): Cmp.Aux[Double, String] =
    Cmp[Double, String](
      (x, y) ⇒
        if (x == 0 || y == 0)
          if (abs(x + y) + 1 <= ε)
            None
          else
            Some(s"(x, y): ($x, $y), ε: $ε")
        else if (new FuzzyCmpOps(x).===(y))
          None
        else
          Some(
            s"(x, y): ($x, $y), ε: $ε; x/y: ${x/y}, y/x: ${y/x}"
          )
    )

  def ===[T, U](t1: T, t2: U)(implicit canEqual: CanEq[T, U]): Unit =
    canEqual
    .cmp(t1, t2)
    .foreach(e ⇒ fail(e.toString))

  def !==[T, U](t1: T, t2: U)(implicit canEqual: CanEq[T, U]): Unit =
    if (canEqual.cmp(t1, t2).isEmpty)
      fail(s"Expected $t1 !== $t2")
}
