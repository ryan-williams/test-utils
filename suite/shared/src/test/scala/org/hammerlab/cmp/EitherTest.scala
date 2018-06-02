package org.hammerlab.cmp

import org.hammerlab.test.Cmp
import shapeless.{ :+:, CNil, Inl, Lazy }

object Helpers {

  import cats.implicits.{ catsKernelStdOrderForInt, catsKernelStdOrderForString }
  import hammerlab.cmp.first._

  def eitherCanEq[L, R, EL, ER](
    implicit
    cmpL: Lazy[Cmp.Aux[L, EL]],
    cmpR: Lazy[Cmp.Aux[R, ER]]
  ):
  Cmp.Aux[
    Either[L, R],
    SealedTrait.Err[
      (EL :+: CNil) :+:
      (ER :+: CNil) :+:
      CNil
    ]
  ] =
    implicitly

  object implicits {
    implicit def eitherProductSerializable[L, R, EL, ER](
      implicit
      cmpL: Lazy[Cmp.Aux[L, EL]],
      cmpR: Lazy[Cmp.Aux[R, ER]],
      pos: Pos
    ):
      Cmp.Aux[
        Either[L, R] with Product with Serializable,
        SealedTrait.Err[
          (EL :+: CNil) :+:
          (ER :+: CNil) :+:
          CNil
        ]
      ] = {
      println(s"**** $pos: eitherProductSerializable")
      CanEq {
        (_, _) ⇒
          println("**** cmp'ing w eitherProductSerializable")
          ???
      }
      //???
      //println("routing via p w s")
      //eitherCanEq[L, R, EL, ER]
    }
  }

  //implicit val esi: Cmp[Either[String, Int]] = ???
//  implicitly[Cmp[Either[String, Int]]]
//  implicitly[Cmp[Either[String, Int]] <:< Cmp[Either[String, Int] with Product with Serializable]]
//  implicitly[Cmp[Either[String, Int] with Product with Serializable]]
}

class EitherTest
  extends hammerlab.Suite {

  import Helpers.implicits.eitherProductSerializable

//  implicitly[Cmp[Either[String, Int]]]
//  implicitly[Cmp[Either[String, Int]] <:< Cmp[Either[String, Int] with Product with Serializable]]
//  implicitly[Cmp[Either[String, Int] with Product with Serializable]]
//  implicitly[Cmp[Product with Serializable with Either[String, Int]]]

  test("simple") {
    val l =  Left("abc")
    val r = Right( 123 )

//    cmp(
//      l,
//      r
//    ) should be(
//      Some(
//        Left("Different types: Left(abc), Right(123)")
//      )
//    )

    implicitly[Cmp[Either[String, Int]]].cmp(Left("abc"), Left("cba"))

//    val err =
//      cmp[Either[String, Int], Either[String, Int]](
//        Left("abc"): Either[String, Int],
//        Left("cba"): Either[String, Int]
//      )
//
//    err should be(
//      Some(
//        Right(
//          Inl(
//            Inl(
//              (
//                "abc",
//                "cba"
//              )
//            )
//          )
//        )
//      )
//    )
  }
}
