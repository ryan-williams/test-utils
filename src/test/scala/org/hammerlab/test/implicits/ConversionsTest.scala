package org.hammerlab.test.implicits

import org.hammerlab.test.Suite
import org.scalactic.ConversionCheckedTripleEquals

case class A(n: Int)
case class B(n: Int)

object B {
  implicit def fooToBar(foo: A): B = B(10 * foo.n)
}

class ConversionsTest
  extends Suite
    with ConversionCheckedTripleEquals {

  implicit def intToString(n: Int): String = s"$n$n"

  val IntToStringConversions = Conversions[Int, String]
  import IntToStringConversions._

  val AToBConversions = Conversions[A, B]
  import AToBConversions._

  test("int⟶string conversions") {
    Option("22") should ===(Some(2))
    ("22", true) should ===((2, true))
    Map("22" → true, "3434" → false) should ===(Map(2 → true, 34 → false))
    List("11", "2323") should ===(Seq(1, 23))
    Array("11", "2323") should ===(Array(1, 23))
    Array("11", "2323") should ===(Seq(1, 23))
    List("22" → true, "3434" → false) should ===(Seq(2 → true, 34 → false))
    Array("22" → true, "3434" → false) should ===(Array(2 → true, 34 → false))
  }

  test("A⟶B conversions") {
    Option(B(10)) should ===(Some(A(1)))
  }
}
