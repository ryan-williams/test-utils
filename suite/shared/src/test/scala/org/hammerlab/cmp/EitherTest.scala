package org.hammerlab.cmp

import shapeless.{ Inl, Inr }

class EitherTest
  extends hammerlab.Suite {

  test("simple") {
    val l =  Left("abc")
    val r = Right( 123 )

    cmp(
      l,
      r
    ) should be(
      Some(
        Left("Different types: Left(abc), Right(123)")
      )
    )

    cmp(
      l: Either[String, Int],
      r: Either[String, Int]
    ) should be(
      Some(
        Left("Different types: Left(abc), Right(123)")
      )
    )

    cmp(
      r,
      l
    ) should be(
      Some(
        Left("Different types: Right(123), Left(abc)")
      )
    )

    cmp(
      r: Either[String, Int],
      l: Either[String, Int]
    ) should be(
      Some(
        Left("Different types: Right(123), Left(abc)")
      )
    )

    ===(l, l)
    ===(r, r)
    !==(l, r)
    !==(r, l)
    !==(l, Left("cba"))
    !==(r, Right(321))

    ===(r: Either[ String, Int], r)
    ===(r: Either[ String, Int], r: Right[ String, Int])

    ===(r:  Right[ String, Int], r)
    ===(r:  Right[Nothing, Int], r)

    cmp(
      Left("abc"),
      Left("cba")
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
      Left("abc"): Either[String, Int],
      Left("cba"): Either[String, Int]
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

    cmp(
      Right(123),
      Right(321)
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
      Right(123): Either[String, Int],
      Right(321): Either[String, Int]
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
