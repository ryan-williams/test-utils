package org.hammerlab.cmp.double

import hammerlab.math.tolerance._
import org.hammerlab.test.Cmp
import org.scalatest.exceptions.TestFailedException

class NeqTest
  extends hammerlab.Suite {
  test("doubles") {
    implicit val ε: E = 1e-6
    val cmp = shapeless.the[Cmp[Double]]

    implicit def liftOpt[T](t: T): Option[T] = Some(t)

    def check(l: Double, r: Double, expected: Option[Neq] = None): Unit = {
      cmp(l, r) should be(expected)
      cmp.eqv(l, r) should be(expected.isEmpty)
    }

    check( 2.0,  2.0         )
    check( 2.0,  2.0000000001)
    check( 2.0,  2.000002    )

    check(-2.0, -2.0         )
    check(-2.0, -2.0000000001)
    check(-2.0, -2.000002    )

    check( 2.0, -2.0         , Neq(2.0, -2.0         , ε))
    check( 2.0, -2.0000000001, Neq(2.0, -2.0000000001, ε))
    check( 2.0, -2.000002    , Neq(2.0, -2.000002    , ε))

    check(-2.0, 2.0         , Neq(-2.0, 2.0         , ε))
    check(-2.0, 2.0000000001, Neq(-2.0, 2.0000000001, ε))
    check(-2.0, 2.000002    , Neq(-2.0, 2.000002    , ε))

    check( 2.0,  2.0000020001, Neq( 2.0,  2.0000020001, ε))
    check(-2.0, -2.0000020001, Neq(-2.0, -2.0000020001, ε))
  }

  tolerance(1e-2)
  test("coarser") {
    ===(2.0, 2.02)
    !==(2.0, 2.02000001)
  }
}
