package org.hammerlab.cmp

import org.hammerlab.cmp.first.Collections.{ Diff, ElemOnly, LeftOnly, RightOnly }

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

  test("iterables") {
    ===(
      Iterable( 1,  6, 22, 17),
      Iterable(11, 66,  2, 77)
    )

    cmp(
      Iterable( 1,  6, 22, 17),
      Iterable(11, 66,  2, 78)
    ) should be(
      Some(
        3 →
          Diff((17, 78))
      )
    )

    ===(
      List( 1,  6, 22, 17): Iterable[Int],
      List(11, 66,  2, 77): Iterable[Int]
    )

    cmp(
      List( 1,  6, 22, 17): Iterable[Int],
      List(11, 66,  2, 78): Iterable[Int]
    ) should be(
      Some(
        3 →
          Diff((17, 78))
      )
    )
  }

  test("seqs") {
    ===(
      Seq( 1,  6, 22, 17),
      Seq(11, 66,  2, 77)
    )

    cmp(
      Seq( 1,  6, 22, 17),
      Seq(11, 66,  2, 78)
    ) should be(
      Some(
        3 →
          Diff((17, 78))
      )
    )

    ===(
      List( 1,  6, 22, 17): Seq[Int],
      List(11, 66,  2, 77): Seq[Int]
    )

    cmp(
      List( 1,  6, 22, 17): Seq[Int],
      List(11, 66,  2, 78): Seq[Int]
    ) should be(
      Some(
        3 →
          Diff((17, 78))
      )
    )
  }

  test("lists") {
    ===(
      List( 1,  6, 22, 17),
      List(11, 66,  2, 77)
    )

    cmp(
      List( 1,  6, 22, 17),
      List(11, 66,  2, 78)
    ) should be(
      Some(
        3 →
          Diff((17, 78))
      )
    )
  }

  test("sets") {
    //===[Set[Int], Set[Int], ElemOnly[Int, Int]](Set(), Set())
    //==[Set[Int], ElemOnly[Int, Int]](Set(), Set())

    ==(Set(): Set[Int])(Set())

    ===(Set(1), Set(1))

    ===(
      Set(  1, 10, 100),
      Set(  1, 10, 100)
    )

    ===(
      Set(  1, 10, 100),
      Set(100,  1,  10)
    )

    cmp(
      Set(  1, 10, 1000, 100),
      Set(100,  1,   10)
    ) should be(
      Some(
        LeftOnly(1000)
      )
    )

    cmp(
      Set(100,  1,   10),
      Set(  1, 10, 1000, 100)
    ) should be(
      Some(
        RightOnly(1000)
      )
    )

    cmp2(
      Set(1)
    )(
      Set( )
    ) should be(
      Some(
        LeftOnly(1)
      )
    )

    !=(Set(1))(Set())
  }
}
