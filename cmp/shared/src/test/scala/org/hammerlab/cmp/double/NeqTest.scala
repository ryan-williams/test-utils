package org.hammerlab.cmp.double

import hammerlab.math.tolerance._
import org.hammerlab.cmp.Cmp

class NeqTest
  extends hammerlab.Suite {

  /** Necessary for comparing [[E]]s */
  implicit val cmpEpsilon: Cmp[E] = Cmp.by(_.ε)

  // TODO: replace [[Option]] with `Opt` that auto-lifts
  implicit def liftOpt[T](t: T): Option[T] = Some(t)

  test("doubles") {
    implicit val ε: E = 1e-6

    def check(l: Double, r: Double, expected: Option[Neq] = None): Unit =
      ==(
        cmp(l, r),
        expected
      )

    check( 2.0,  2.0         )
    check( 2.0,  2.0000000001)
    check( 2.0,  2.000002    )

    check(-2.0, -2.0         )
    check(-2.0, -2.0000000001)
    check(-2.0, -2.000002    )

    check( 2.0, -2.0         , Neq(2.0, -2.0         , ε))
    check( 2.0, -2.0000000001, Neq(2.0, -2.0000000001, ε))
    check( 2.0, -2.000002    , Neq(2.0, -2.000002    , ε))

    check(-2.0,  2.0         , Neq(-2.0, 2.0         , ε))
    check(-2.0,  2.0000000001, Neq(-2.0, 2.0000000001, ε))
    check(-2.0,  2.000002    , Neq(-2.0, 2.000002    , ε))

    check( 2.0,  2.0000020001, Neq( 2.0,  2.0000020001, ε))
    check(-2.0, -2.0000020001, Neq(-2.0, -2.0000020001, ε))
  }

  tolerance(1e-2)
  test("coarser") {
    ==(2.0, 2.02)
    !=(2.0, 2.02000001)
  }
}
