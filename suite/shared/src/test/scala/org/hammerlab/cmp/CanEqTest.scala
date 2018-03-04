package org.hammerlab.cmp

import org.hammerlab.cmp.double.Neq
import org.hammerlab.test.Cmp
import org.scalatest.exceptions.TestFailedException
import shapeless.{ Inl, Inr }

sealed trait T[R] extends Product with Serializable
case class D(d: Double) extends T[Double]
case class E(d: Double) extends T[Double]

case class Complex(r: Double, i: Double)

class CanEqTest
  extends hammerlab.Suite {

  test("seq") {
    ===(
      Seq(2.0, 3.0),
      Seq(2.0000000001, 3.0)
    )
    intercept[TestFailedException] {
      !==(
        Seq(2.0, 3.0),
        Seq(2.0000000001, 3.0)
      )
    }

    ===(
      Seq(2.0, 3.0),
      Seq(2.000002, 3.0)
    )

    !==(
      Seq(2.0, 3.0),
      Seq(2.000002001, 3.0)
    )
    intercept[TestFailedException] {
      ===(
        Seq(2.0, 3.0),
        Seq(2.000002001, 3.0)
      )
    }

    ===(
      Seq(2.0, 3.0),
      Seq(2.0, 3.0000029999999995)
    )

    !==(
      Seq(2.0, 3.0),
      Seq(2.0, 3.000003001)
    )

    ===(
      Seq(-2.0, 3.0),
      Seq(-2.000002, 3.0)
    )

    !==(
      Seq(-2.0, 3.0),
      Seq(-2.000002001, 3.0)
    )

    ===(
      Seq(2.0, -3.0),
      Seq(2.0, -3.0000029999999995)
    )

    !==(
      Seq(2.0, -3.0),
      Seq(2.0, -3.000003001)
    )

    !==(
      Seq(2, 3, 4),
      Seq(2, 3)
    )

    !==(
      Seq(2, 3),
      Seq(2, 3, 4)
    )

    val lt: Seq[T[Double]] = Seq(D(2.0), D(3.0))
    val rt: Seq[T[Double]] = Seq(D(2.0), D(3.0))

    ===(
      Seq(D(2.0), D(3.0)),
      Seq(D(2.0), D(3.0))
    )

    ===(lt, rt)

    !==(
      Seq(D(2.0), E(3.0)),
      Seq(D(2.0), D(3.0))
    )

    !==(
      Seq(D(2.0), D(3.0)),
      Seq(D(2.0), E(3.0))
    )

    !==(
      Seq(E(2.0), D(3.0)),
      Seq(D(2.0), D(3.0))
    )

    !==(
      Seq(D(2.0), D(3.0)),
      Seq(E(2.0), D(3.0))
    )
  }

  test("strings") {
    ===("abc", "abc")
    !==("abc", "abcd")
    !==("abcd", "abc")
    ===("", "")
    !==("abc", "")
    !==("", "abc")
  }

  test("custom cmp") {
    implicit val cmp = Cmp.by[Int, String](_.toInt)
    ===("2", "2")
    ===("02", "2")
    !==("-02", "2")
    !==("03", "2")
    !==("3", "2")
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
