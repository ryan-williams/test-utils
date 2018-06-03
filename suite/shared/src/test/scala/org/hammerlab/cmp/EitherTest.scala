package org.hammerlab.cmp

import org.hammerlab.test.Cmp
import shapeless._

class EitherTest
  extends hammerlab.Suite {

  val l1 =  Left("abc")
  val l2 =  Left("cba")

  val r1 = Right( 123 )
  val r2 = Right( 321 )

  test("different types") {
    cmp(
      l1: Either[String, Int],
      r1
    ) should be(
      Some(
        Left("Different types: Left(abc), Right(123)")
      )
    )

    cmp(
      l1: Either[String, Int],
      r1: Either[String, Int]
    ) should be(
      Some(
        Left("Different types: Left(abc), Right(123)")
      )
    )

    cmp(
      r1: Either[String, Int],
      l1
    ) should be(
      Some(
        Left("Different types: Right(123), Left(abc)")
      )
    )

    cmp(
      r1: Either[String, Int],
      l1: Either[String, Int]
    ) should be(
      Some(
        Left("Different types: Right(123), Left(abc)")
      )
    )

    !=(l1: Either[String, Int], r1)
    !=(r1: Either[String, Int], l1)
  }

  test("equal lefts") {
    ==(l1, l1)
    ==(l2, l2)

    ==(l1: Either[String,     Int], l1: Either[String, Int])
    ==(l2: Either[String,     Int], l2: Either[String, Int])

    ==(l1: Either[String,     Int], l1                     )
    ==(l1: Either[String,     Int], l1:   Left[String, Int])

    ==(l1:   Left[String,     Int], l1                     )
    ==(l1:   Left[String, Nothing], l1                     )

    ==(cmp(l1, l1), None)
    ==(cmp(l2, l2), None)
  }

  test("equal rights") {
    ==(r1, r1)
    ==(r2, r2)

    ==(r1: Either[ String, Int], r1: Either[String, Int])
    ==(r2: Either[ String, Int], r2: Either[String, Int])

    ==(r1: Either[ String, Int], r1                     )
    ==(r1: Either[ String, Int], r1:  Right[String, Int])

    ==(r1:  Right[ String, Int], r1                     )
    ==(r1:  Right[Nothing, Int], r1                     )

    ==(cmp(r1, r1), None)
    ==(cmp(r2, r2), None)
  }

  test("different lefts") {
    !=(l1, l2)
    !=(l2, l1)

    cmp(
      l1,
      l2
    ) should be(
      Some(
        Inl(
          (
            "abc",
            "cba"
          )
        )
      )
    )

    cmp(
      l1: Either[String, Int],
      l2: Either[String, Int]
    ) should be(
      Some(
        Right(
          Inl(
            Inl(
              (
                "abc",
                "cba"
              )
            )
          )
        )
      )
    )
  }

  test("different rights") {
    !=(r1, r2)
    !=(r2, r1)

    cmp(
      r1,
      r2
    ) should be(
      Some(
        Inl(
          (
            123,
            321
          )
        )
      )
    )

    cmp(
      r1: Either[String, Int],
      r2: Either[String, Int]
    ) should be(
      Some(
        Right(
          Inr(
            Inl(
              Inl(
                (
                  123,
                  321
                )
              )
            )
          )
        )
      )
    )
  }
}
