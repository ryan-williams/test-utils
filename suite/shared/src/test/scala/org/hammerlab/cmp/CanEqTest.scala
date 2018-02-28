package org.hammerlab.cmp

import org.hammerlab.cmp.double.Neq
import org.hammerlab.test.Cmp
import shapeless.{ :+:, CNil, Inl, Inr }

sealed trait T[R]
case class D(d: Double) extends T[Double]

case class Complex(r: Double, i: Double)

class CanEqTest
  extends hammerlab.Suite {

  test("seq") {
    ===(
      Seq(2.0, 3.0),
      Seq(2.0000000001, 3.0)
    )

    ===(
      Seq(2.0, 3.0),
      Seq(2.000002, 3.0)
    )

    !==(
      Seq(2.0, 3.0),
      Seq(2.000002001, 3.0)
    )

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

    val lt: Seq[T[Double]] = Seq(D(2.0), D(3.0))
    val rt: Seq[T[Double]] = Seq(D(2.0), D(3.0))

    ===(
      Seq(D(2.0), D(3.0)),
      Seq(D(2.0), D(3.0))
    )

    ===(lt, rt)
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
