package org.hammerlab

import cats.Show.ContravariantShow
import cats.{ Eq, Show }
import hammerlab.math.tolerance._
import org.hammerlab.cmp.CanEq.withConversion
import org.hammerlab.cmp.{ CanEq, double }
import org.hammerlab.test.{ Afters, Befores, matchers }
import org.scalatest.{ FunSuite, Matchers }

abstract class Suite
  extends FunSuite
     with Matchers
     with Befores
     with Afters
     with matchers.lines.HasMatcher
     with matchers.seqs.all
     with double.HasNeq {

  /** Fuzziness for [[Double]] assertions / equality-comparisons; see [[doubleCmp]] */
  implicit var ε: E = 1e-6

  /** Convenience-setter for [[ε]] */
  def tolerance(d: Double): Unit = {
    ε = d
  }

  implicit val    intOrder: Eq[   Int] = cats.instances.   int.catsKernelStdOrderForInt
  implicit val   longOrder: Eq[  Long] = cats.instances.  long.catsKernelStdOrderForLong
  implicit val stringOrder: Eq[String] = cats.instances.string.catsKernelStdOrderForString

  /** Default [[CanEq]] instances for [[Double]]s vs. [[Int]]s */
  implicit val int2double : CanEq[Double, Int] = withConversion[Double, Int]
  implicit val int2long   : CanEq[  Long, Int] = withConversion[  Long, Int]

  private def showAny[T] =
    new Show[T] {
      override def show(t: T): String = t.toString
    }

  def ===[T, U, E](t1: T, t2: U)(implicit cmp: CanEq.Aux[T, U, E], showError: Show[E] = showAny[E]): Unit =
    cmp(t1, t2)
      .foreach {
        e ⇒
          fail(
            showError.show(e)
          )
      }

  def !==[T, U](t1: T, t2: U)(implicit cmp: CanEq[T, U]): Unit =
    if (cmp(t1, t2).isEmpty)
      fail(s"Expected $t1 !== $t2")
}
