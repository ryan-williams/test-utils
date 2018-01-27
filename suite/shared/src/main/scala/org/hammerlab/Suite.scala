package org.hammerlab

import cats.Eq
import cats.instances.{ LongInstances, StringInstances }
import cats.kernel.instances.ListInstances2
import hammerlab.math.syntax.Tolerance
import org.hammerlab.test.CanEq.fromExisting
import org.hammerlab.test.{ CanEq, MkEqDerivation }
import org.scalatest.{ BeforeAndAfterAll, BeforeAndAfterEach, FunSuite, Matchers }

import scala.collection.mutable.ArrayBuffer

abstract class Suite
  extends FunSuite
    with Matchers
    with BeforeAndAfterAll
    with BeforeAndAfterEach
    with MkEqDerivation
    with StringInstances
    with ListInstances2
    with LongInstances {

  implicit var ε = Tolerance(1e-6)

  def tolerance(d: Double): Unit = {
    ε = d
  }

  implicit val int2double : CanEq[Double, Int] = fromExisting[Double, Int]
  implicit val int2long   : CanEq[  Long, Int] = fromExisting[  Long, Int]

  implicit def doubleEq(implicit tolerance: Tolerance): Eq[Double] =
    new Eq[Double] {
      override def eqv(x: Double, y: Double) =
        x + tolerance >= y &&
        y + tolerance >= x
    }

  def ===[T, U](t1: T, t2: U)(implicit canEqual: CanEq[T, U]): Unit = {
    if (!canEqual.eqv(t1, t2))
      fail(s"$t1 didn't match $t2")
  }

  private val befores = ArrayBuffer[() ⇒ Unit]()

  def before(fn: ⇒ Unit): Unit = {
    befores += (() ⇒ fn)
  }

  final override def beforeEach(): Unit = {
    super.beforeEach()
    for { beforeFn ← befores } { beforeFn() }
  }

  private val afters = ArrayBuffer[() ⇒ Unit]()

  def after(fn: ⇒ Unit): Unit = {
    afters += (() ⇒ fn)
  }

  final override def afterEach(): Unit = {
    super.afterEach()
    for { afterFn ← afters } { afterFn() }
  }
}
