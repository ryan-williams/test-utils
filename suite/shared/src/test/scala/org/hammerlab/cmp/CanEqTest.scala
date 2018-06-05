package org.hammerlab.cmp

import org.hammerlab.cmp.double.Neq
import org.hammerlab.cmp.first.Collections.Diff
import org.hammerlab.test.Cmp
import org.scalatest.exceptions.TestFailedException
import shapeless._

sealed trait T extends Product with Serializable
case class D(d: Double) extends T
case class E(d: Double) extends T
case class F(d: Double) extends T

case class Complex(r: Double, i: Double)

class CanEqTest
  extends hammerlab.Suite {

  test("seq") {
    ==(
      Seq(2.0, 3.0),
      Seq(2.0000000001, 3.0)
    )
    intercept[TestFailedException] {
      !=(
        Seq(2.0, 3.0),
        Seq(2.0000000001, 3.0)
      )
    }

    ==(
      Seq(2.0, 3.0),
      Seq(2.000002, 3.0)
    )

    !=(
      Seq(2.0, 3.0),
      Seq(2.000002001, 3.0)
    )
    intercept[TestFailedException] {
      ==(
        Seq(2.0, 3.0),
        Seq(2.000002001, 3.0)
      )
    }

    ==(
      Seq(2.0, 3.0),
      Seq(2.0, 3.0000029999999995)
    )

    !=(
      Seq(2.0, 3.0),
      Seq(2.0, 3.000003001)
    )

    ==(
      Seq(-2.0, 3.0),
      Seq(-2.000002, 3.0)
    )

    !=(
      Seq(-2.0, 3.0),
      Seq(-2.000002001, 3.0)
    )

    ==(
      Seq(2.0, -3.0),
      Seq(2.0, -3.0000029999999995)
    )

    !=(
      Seq(2.0, -3.0),
      Seq(2.0, -3.000003001)
    )

    !=(
      Seq(2, 3, 4),
      Seq(2, 3)
    )

    !=(
      Seq(2, 3),
      Seq(2, 3, 4)
    )

    implicitly[Cmp[T]]
    implicitly[Cmp[Seq[T]]]

    val lt: Seq[T] = Seq(D(2.0), D(3.0))
    val rt: Seq[T] = Seq(D(2.0), D(3.0))

    ==(
      Seq(D(2.0), D(3.0)),
      Seq(D(2.0), D(3.0))
    )

    ==(lt, rt)

    !=(
      Seq(D(2.0), E(3.0))
    )(
      Seq(D(2.0), D(3.0))
    )

    !=(
      Seq[T](D(2.0), D(3.0)),
      Seq(D(2.0), E(3.0))
    )

    !=(
      Seq(E(2.0), D(3.0)),
      Seq(D(2.0), D(3.0))
    )

    !=(
      Seq[T](D(2.0), D(3.0)),
      Seq(E(2.0), D(3.0))
    )
  }

  test("different types in coproduct tail") {
    cmp(
      Seq(D(2.0), E(2.9))
    )(
      Seq(D(2.0), F(3.1))
    ) should be(
      Some(
        (
          1,
          Diff(
            Left(
              "Different types: E(2.9), F(3.1)"
            )
          )
        )
      )
    )
  }

  test("numeric conversions") {
    ==(2, 2)
    !=(3, 2)

    ==(2L, 2)
    !=(3L, 2)

    ==(2, 2L)
    !=(3, 2L)

    ==(2L, 2L)
    !=(3L, 2L)
  }

  test("strings") {
    ==("abc", "abc")
    !=("abc", "abcd")

    val css = implicitly[Cmp[String]]
    ==(css.eqv("abc", "abc" ), true)
    ==(css.eqv("abc", "abcd"), false)

    ==("abc", "abc")
    !=("abc", "abcd")

    !=("abcd", "abc")
    ==("", "")
    !=("abc", "")
    !=("", "abc")
  }

  test("custom cmp") {
    implicit val cmp = Cmp.by[Int, String](_.toInt)
    ==("2", "2")
    ==("02", "2")
    !=("-02", "2")
    !=("03", "2")
    !=("3", "2")

    {
      implicit val cmp = CanEq.by[String, Int](_.length)
      ==( "", 0)
      !=( "", 1)
      !=(" ", 0)
      ==("abc", 3)
    }
  }

  test("case class") {
    val cmp = shapeless.the[Cmp[Complex]]
    cmp(
      Complex(2.0, 3.0),
      Complex(2.0, 3.0)
    ) should be(
      None
    )

    cmp(
      Complex(2.0, 3.0),
      Complex(2.000002001, 3.0)
    ) should be(
      Some(
        Inl(
          Neq(
            2.0,
            2.000002001,
            ε
          )
        )
      )
    )

    cmp(
      Complex(2.0, 3.0),
      Complex(2.0, 3.000003)
    ) should be(
      Some(
        Inr(
          Inl(
            Neq(
              3.0,
              3.000003,
              ε
            )
          )
        )
      )
    )
  }
}
