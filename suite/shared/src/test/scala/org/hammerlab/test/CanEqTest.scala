package org.hammerlab.test

//import org.hammerlab.test.CanEq.Eqv
import shapeless._

sealed trait T[R]
case class D[R](r: R) extends T[R]

case class Complex(r: Double, i: Double)

class CanEqTest
  extends org.hammerlab.Suite {

  implicitly[CanEq[Double, Double]]

  {
    type H = Double
    type T = HNil
    type ET = CNil
    implicitly[Lazy[CanEq[H, H]]]

    implicitly[CanEq.Aux[T, T, ET]]

    //implicitly[Lazy[CanEq.Aux[T, T, ET]]]
//    implicitly[Generic.Aux[T, L]]
//    implicitly[Lazy[CanEq.Aux[L, L, E]]]
  }

  implicitly[CanEq[Double :: HNil, Double :: HNil]]

  implicitly[CanEq[Seq[Double], Seq[Double]]]

//  implicitly[Eqv[Complex]]
  implicitly[CanEq[Complex, Complex]]

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

    implicitly[CanEq[T[Double], T[Double]]]
    implicitly[CanEq[Seq[T[Double]], Seq[T[Double]]]]

    ===(
      Seq(D(2.0), D(3.0)),
      Seq(D(2.0), D(3.0))
    )

    ===(lt, rt)
  }
}
