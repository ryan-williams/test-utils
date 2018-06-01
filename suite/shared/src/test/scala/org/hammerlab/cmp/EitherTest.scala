package org.hammerlab.cmp

import shapeless.Inl

class EitherTest
  extends hammerlab.Suite {

  test("simple") {
    val l =  Left("abc")
    val r = Right( 123 )

    val err =
      cmp(
        Left("abc"): Either[String, Int],
        Left("cba"): Either[String, Int]
      )
      .get

    println(err)

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
  }
}
