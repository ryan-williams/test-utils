package org.hammerlab.test

import java.{lang ⇒ jl}

import org.hammerlab.test.files.TmpFiles
import org.scalactic.{ CanEqual, ConversionCheckedTripleEquals }
import org.scalatest.{ BeforeAndAfter, BeforeAndAfterAll, FunSuite, Matchers }

// Simple wrapper for common test-suite boilerplate.
class Suite
  extends FunSuite
    with Matchers
    with ConversionCheckedTripleEquals
    with BeforeAndAfterAll
    with BeforeAndAfter
    with TmpFiles {

  def obviousEquality[L, R]: CanEqual[L, R] =
    new CanEqual[L, R] {
      override def areEqual(a: L, b: R): Boolean = a == b
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
