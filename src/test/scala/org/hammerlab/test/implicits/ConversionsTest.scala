package org.hammerlab.test.implicits

import org.hammerlab.test.Suite
import org.scalactic.ConversionCheckedTripleEquals

object Foo {
  case class A(n: Int)
  case class B(n: Int)

  object B {
    implicit def fooToBar(foo: A): B = B(10 * foo.n)
  }
}

import Foo.{A, B}

@Conversions[Int, String]
trait IntStringConversions {
  val nonce1 = 111
  implicit def intToString(n: Int): String = s"$n$n"
  def nonce2 = "222"
}

@Conversions[A, B]
trait ABConversions {
  implicit lazy val nonce3 = ("aaa", 333, true)
}

// Macro-annotations on top-level class-/trait-declarations have some subtleties around companion-object-handling; test
// those using this object.
object ABConversions {
  val nonce4 = 444
}

class ConversionsTest
  extends Suite
    with ConversionCheckedTripleEquals
    with IntStringConversions
    with ABConversions {

  test("int⟶string conversions") {
    Option("22") should ===(Some(2))
    ("22", true) should ===((2, true))
    Map("22" → true, "3434" → false) should ===(Map(2 → true, 34 → false))
    Vector("11", "2323") should ===(Seq(1, 23))

    Vector(Option("11"), None, Option("2323")) should ===(Seq(Some(1), None, Some(23)))
    Array("11", "2323") should ===(Array(1, 23))
    Array("11", "2323") should ===(Seq(1, 23))
    Vector("22" → true, "3434" → false) should ===(Seq(2 → true, 34 → false))
    Array("22" → true, "3434" → false) should ===(Array(2 → true, 34 → false))
    ("22", "33") should ===(2, 3)
    ("22", "33", true) should ===(2, 3, true)

    Seq("22", "33") should ===(Seq(2, 3))
    Vector("22", "33") should ===(Seq(2, 3))
  }

  test("A⟶B conversions") {
    Option(B(10)) should ===(Some(A(1)))
  }

  test("annotated trait bodies are preserved") {
    nonce1 should be(111)
    nonce2 should be("222")
    implicitly[(String, Int, Boolean)] should be("aaa", 333, true)
    ABConversions.nonce4 should be(444)
  }
}
