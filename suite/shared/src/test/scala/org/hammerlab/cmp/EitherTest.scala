package org.hammerlab.cmp

import shapeless.Inl

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

    val err =
      cmp[Either[String, Int], Either[String, Int]](
        Left("abc"): Either[String, Int],
        Left("cba"): Either[String, Int]
      )

    err should be(
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
}
