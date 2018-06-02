package org.hammerlab.cmp.bug

import org.hammerlab.cmp.Pos
import org.hammerlab.test.Cmp
import shapeless._

trait TC[-T]

sealed trait A
case class A1(n: Int) extends A
case class A2(n: Int) extends A

object Test {
  import cats.implicits.catsKernelStdOrderForInt

  implicit def caseclass[T, L <: HList](
    implicit
    gen: Generic.Aux[T, L],
    list: Lazy[TC[L]]
  ):
    TC[T] = ???

  implicit def cons[H, T <: HList](
    implicit
    head: Lazy[TC[H]],
    tail: Lazy[TC[T]]
  ):
    TC[H :: T] = ???

  implicit def hnil: TC[HNil] = ???

  implicit def sealedtrait[T, L <: Coproduct](
    implicit
    gen: Generic.Aux[T, L],
    list: Lazy[TC[L]],
    pos: Pos
  ):
    TC[T] = {
    println(s"$pos: sealedtrait")
    new TC[T] {}
  }

  implicit def ccons[H, T <: Coproduct](
    implicit
    head: Lazy[TC[H]],
    tail: Lazy[TC[T]],
    pos: Pos
  ):
    TC[H :+: T] = {
    println(s"$pos: ccons")
    new TC[H :+: T] {}
  }

  implicit def cnil: TC[CNil] = ???

  implicit def int: TC[Int] = ???

  import cats.implicits.catsKernelStdOrderForInt
  import hammerlab.cmp.first._

//  import hammerlab.cmp.first.caseclass._
//  import hammerlab.cmp.first.sealedtrait._
  //implicit val a: Cmp[A] = ???

  //implicitly[TC[A]]

  object helpers {
    implicit def productWithSerializable: TC[A with Product with Serializable] = ???
    implicit def cmpProductWithSerializable: Cmp[A with Product with Serializable] = ???
  }

  def main(args: Array[String]): Unit = {
    if (args.length > 0) {
      println("yay")
      import helpers._
//      import Test.caseclass
//      implicitly[TC[A]]
//      implicitly[Cmp[A]]
      implicitly[Cmp[A1]]
    } else {
      println("boo")
//      implicitly[TC[A]]
//      implicitly[Cmp[A]]
      implicitly[Cmp[A1]]
    }
  }

//  implicitly[TC[A] <:< TC[A with Product]]
//  implicitly[TC[A with Product]]
//
//  implicitly[TC[A] <:< TC[A with Serializable]]
//  implicitly[TC[A with Serializable]]
//
//  implicitly[TC[A] <:< TC[A with Product with Serializable]]
//  implicitly[TC[A with Product with Serializable]]
}
