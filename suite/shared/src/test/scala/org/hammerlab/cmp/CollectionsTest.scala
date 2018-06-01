package org.hammerlab.cmp

import hammerlab.cmp.first._

class CollectionsTest
  extends hammerlab.Suite {

  override implicit val intOrder: cats.Eq[Int] =
    new cats.Eq[Int] {
      def eqv(x: Int, y: Int): Boolean =
        x % 10 == y % 10
    }

  test("maps") {
    ===(
      Map("a" → 2),
      Map("a" → 2)
    )

    ===(
      Map("a" →  2),
      Map("a" → 12)
    )

    cmp(
      Map("a" → 2),
      Map("a" → 3)
    ) should be(
      Some(
        (
          "a",
          Diff((2, 3))
        )
      )
    )

    cmp(
      Map("a" → 2),
      Map("b" → 2)
    ) should be(
      Some(
        (
          "a",
          LeftOnly(2)
        )
      )
    )

    cmp(
      Map("a" → 2),
      Map("a" → 2,
          "b" → 2)
    ) should be(
      Some(
        (
          "b",
          RightOnly(2)
        )
      )
    )

    cmp(
      Map("a" → 2,
          "b" → 2),
      Map("a" → 2)
    ) should be(
      Some(
        (
          "b",
          LeftOnly(2)
        )
      )
    )
  }

  test("arrays") {
    ===(
      Array( 1,  6, 22, 17),
      Array(11, 66,  2, 77)
    )

    cmp(
      Array( 1,  6, 22, 17),
      Array(11, 66,  2, 78)
    ) should be(
      Some(
        3 →
          Diff((17, 78))
      )
    )
  }
}
