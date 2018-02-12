package org.hammerlab.test

import java.{ lang ⇒ jl }

/**
 * Mix-in with [[org.scalactic.CanEqual]] helpers\
 */
trait CanEqual {

  def obviousEquality[L, R]: org.scalactic.CanEqual[L, R] =
    new org.scalactic.CanEqual[L, R] {
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
