package org.hammerlab.cmp

import hammerlab.cmp.first._
import org.hammerlab.test.Cmp
import CanEq.cmp

class CollectionsTest
  extends hammerlab.Suite {
  test("maps") {
    ===(
      Map("a" → 2),
      Map("a" → 2)
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

  test("custom value cmp") {
    val intCmp = implicitly[Cmp[Int]]

    {
      implicit val lastDigitCmp = Cmp.by[Int, Int](_ % 10)(intCmp)
      ===(
        Map("a" →  2),
        Map("a" → 12)
      )

      cmp(
        Map("a" →  2),
        Map("a" →  3)
      ) should be(
        Some(
          (
            "a",
            Diff((2, 3))
          )
        )
      )
    }
  }
}
