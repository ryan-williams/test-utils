package org.hammerlab.cmp.double

import org.hammerlab.math.syntax.E
import org.hammerlab.test.Cmp

class NeqTest
  extends hammerlab.Suite {
  test("doubles") {
    implicit val ε: E = 1e-6
    val cmp = shapeless.the[Cmp[Double]]

    cmp( 2.0,  2.0         ) should be(None)
    cmp( 2.0,  2.0000000001) should be(None)
    cmp( 2.0,  2.000002    ) should be(None)

    cmp(-2.0, -2.0         ) should be(None)
    cmp(-2.0, -2.0000000001) should be(None)
    cmp(-2.0, -2.000002    ) should be(None)

    cmp( 2.0, -2.0         ) should be(Some(Neq(2.0, -2.0         , ε)))
    cmp( 2.0, -2.0000000001) should be(Some(Neq(2.0, -2.0000000001, ε)))
    cmp( 2.0, -2.000002    ) should be(Some(Neq(2.0, -2.000002    , ε)))

    cmp(-2.0, 2.0         ) should be(Some(Neq(-2.0, 2.0         , ε)))
    cmp(-2.0, 2.0000000001) should be(Some(Neq(-2.0, 2.0000000001, ε)))
    cmp(-2.0, 2.000002    ) should be(Some(Neq(-2.0, 2.000002    , ε)))

    cmp( 2.0,  2.0000020001) should be(Some(Neq( 2.0,  2.0000020001, ε)))
    cmp(-2.0, -2.0000020001) should be(Some(Neq(-2.0, -2.0000020001, ε)))
  }
}
