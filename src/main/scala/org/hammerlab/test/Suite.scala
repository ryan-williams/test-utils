package org.hammerlab.test

import java.{ lang ⇒ jl }

import org.hammerlab.test.files.TmpFiles
import org.scalactic.{ CanEqual, ConversionCheckedTripleEquals }
import org.scalatest.{ BeforeAndAfterAll, BeforeAndAfterEach, FunSuite, Matchers }

import scala.collection.mutable.ArrayBuffer

// Simple wrapper for common test-suite boilerplate.
class Suite
  extends FunSuite
    with Matchers
    with ConversionCheckedTripleEquals
    with BeforeAndAfterAll
    with BeforeAndAfterEach
    with TmpFiles {

  def obviousEquality[L, R]: CanEqual[L, R] =
    new CanEqual[L, R] {
      override def areEqual(a: L, b: R): Boolean = a == b
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

  // Some implicits to allow trivial conversions for type-safe equality checking with ===.
  implicit val intLongEqual = obviousEquality[Int, Long]
  implicit val longIntEqual = obviousEquality[Long, Int]
  implicit val jlongIntEqual = obviousEquality[jl.Long, Int]
  implicit val jlongLongEqual = obviousEquality[jl.Long, Long]
  implicit val longJLongEqual = obviousEquality[Long, jl.Long]
  implicit val integerIntEqual = obviousEquality[Integer, Int]
  implicit val jboolBoolEqual = obviousEquality[jl.Boolean, Boolean]
  implicit val doubleFloatEqual = obviousEquality[jl.Double, Float]
  implicit val jfloatFloatEqual = obviousEquality[jl.Float, Float]
}
