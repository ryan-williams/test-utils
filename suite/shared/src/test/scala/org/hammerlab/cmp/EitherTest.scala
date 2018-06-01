package org.hammerlab.cmp

import org.hammerlab.test.Cmp
import shapeless.{ :+:, CNil, Generic, HNil, Inl, Inr, Lazy, :: }

object Helpers {
//  implicit def canEqSuperclass[T, U <: T](implicit cmp: Cmp[T]): CanEq.Aux[T, U, cmp.Error] =
//    CanEq {
//      (t, u) ⇒
//        cmp(t, u)
//    }

  import hammerlab.cmp.{first ⇒ f}
  //import hammerlab.cmp.first._

//  def eitherProductSerializable[L, R](implicit cmp: Lazy[Cmp[Either[L, R]]]): Cmp.Aux[Either[L, R] with Product with Serializable, cmp.value.Error] =
//    CanEq {
//      (l, r) ⇒
//        cmp.value(l, r)
//    }

  import cats.implicits.{catsKernelStdOrderForInt, catsKernelStdOrderForString }

  implicitly[Cmp[String]]
  implicitly[Cmp[Int]]
  implicitly[Cmp[Either[String, Int]]]
  implicitly[Lazy[Cmp[Either[String, Int]]]]

  def eitherCanEq[L, R](
    implicit
    cmpL: Cmp[L],
    cmpR: Cmp[R]
  ):
  Cmp.Aux[
    Either[L, R],
    Either[
      String,
      (cmpL.Error :+: CNil) :+:
      (cmpR.Error :+: CNil) :+:
      CNil
    ]
  ] = {

    implicit val cl: Cmp.Aux[L, cmpL.Error] = cmpL
    implicit val cr: Cmp.Aux[R, cmpR.Error] = cmpR

//    implicitly[Cmp[L]]
//    implicitly[
//      Cmp.Aux[
//        L,
//        cmpL.Error
//      ]
//    ]

//    implicitly[Cmp[L :: HNil]]
//    implicitly[
//      Cmp.Aux[
//        L :: HNil,
//        cmpL.Error :+: CNil
//      ]
//    ]

    implicit val cll: Cmp.Aux[
      L :: HNil,
      cmpL.Error :+: CNil
    ] = f.cmpCons[L, HNil, CNil]

    implicit val clr: Cmp.Aux[
      R :: HNil,
      cmpR.Error :+: CNil
    ] = f.cmpCons[R, HNil, CNil]

    implicit val cleft: Cmp.Aux[
      Left[L, R],
      cmpL.Error :+: CNil
    ] = f.cmpCaseClass[Left[L, R], L :: HNil, cmpL.Error :+: CNil]

    implicit val cright: Cmp.Aux[
      Right[L, R],
      cmpR.Error :+: CNil
    ] = f.cmpCaseClass[Right[L, R], R :: HNil, cmpR.Error :+: CNil]

//    implicitly[Cmp[Left[L, R]]]
//    implicitly[
//      Cmp.Aux[
//        Left[L, R],
//        cmpL.Error :+: CNil
//      ]
//    ]
//
//    implicitly[Cmp[Right[L, R]]]
//    implicitly[
//      Cmp.Aux[
//        Right[L, R],
//        cmpR.Error :+: CNil
//      ]
//    ]

    implicit val ceithercp: Cmp.Aux[
       Left[L, R] :+:
      Right[L, R] :+:
       CNil,
      Either[
        String,
        (cmpL.Error :+: CNil) :+:
        (cmpR.Error :+: CNil) :+:
        CNil
      ]
    ] =
      f.cmpCCons[
         Left[L, R],
        Right[L, R] :+:
         CNil,
        (cmpR.Error :+: CNil) :+:
        CNil
      ]

//    implicitly[
//      Cmp[
//         Left[L, R] :+:
//        Right[L, R] :+:
//        CNil
//      ]
//    ]
//    implicitly[
//      Cmp.Aux[
//         Left[L, R] :+:
//        Right[L, R] :+:
//        CNil,
//        Either[
//          String,
//          (cmpL.Error :+: CNil) :+:
//          (cmpR.Error :+: CNil) :+:
//          CNil
//        ]
//      ]
//    ]

    hammerlab.cmp.first.cmpSealedTrait[
      Either[L, R],
      Left[L, R] :+:
      Right[L, R] :+:
      CNil,
      (cmpL.Error :+: CNil) :+:
      (cmpR.Error :+: CNil) :+:
      CNil
    ]

    //    implicit val cp: Cmp.Aux[
//      Left[L, R] :+:
//      Right[L, R] :+:
//      CNil,
//      Either[
//        String,
//        cmpL.Error :+:
//        cmpR.Error :+:
//        CNil
//      ]
//    ] = ???

//    hammerlab.cmp.first.cmpSealedTrait[
//      Either[L, R],
//        Left[L, R] :+:
//       Right[L, R] :+:
//              CNil,
//      cmpL.Error :+:
//      cmpR.Error :+:
//      CNil
//    ]
  }


//  def eitherCanEq[L, R](
//    implicit
//    cmpL: Lazy[Cmp[L]],
//    cmpR: Lazy[Cmp[R]]
//  ):
//    Cmp.Aux[
//      Either[L, R],
//      Either[
//        String,
//        cmpL.value.Error :+:
//        cmpR.value.Error :+:
//        CNil
//      ]
//    ] =
//    shapeless.the[
//      Cmp.Aux[
//        Either[L, R],
//        Either[
//          String,
//          cmpL.value.Error :+:
//          cmpR.value.Error :+:
//          CNil
//        ]
//      ]
//    ]

//    hammerlab.cmp.first.cmpSealedTrait[
//      Either[L, R],
//        Left[L, R] :+:
//       Right[L, R] :+:
//              CNil,
//      cmpL.value.Error :+:
//      cmpR.value.Error :+:
//      CNil
//    ]
}

class EitherTest
  extends hammerlab.Suite {

  //implicitly[Generic.Aux[Either[String, Int], Left[String, Int] :+: Right[String, Int] :+: CNil]]

  //import Helpers.canEqSuperclass

//  implicit def eitherProductSerializable[L, R](
//    implicit
//    cmpL: Lazy[Cmp[L]],
//    cmpR: Lazy[Cmp[R]]
//  ):
//    Cmp.Aux[
//      Either[L, R] with Product with Serializable,
//      Either[
//        String,
//        cmpL.value.Error :+:
//        cmpR.value.Error :+:
//        CNil
//      ]
//    ] =
//    Helpers.eitherCanEq[L, R](cmpL, cmpR)

  implicit def eitherProductSerializable[L, R](
    implicit
    cmpL: Cmp[L],
    cmpR: Cmp[R]
  ):
  Cmp.Aux[
    Either[L, R] with Product with Serializable,
    Either[
      String,
      (cmpL.Error :+: CNil) :+:
      (cmpR.Error :+: CNil) :+:
      CNil
    ]
  ] =
    Helpers.eitherCanEq[L, R](cmpL, cmpR)


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
        )
      )
    )
  }
}
