package org.hammerlab.cmp.all

import org.hammerlab.cmp.{ CanEq, Cmp }
import org.hammerlab.cmp.all.Test._
import org.hammerlab.shapeless.neohlist.{ NeoHList, OHList }
import shapeless._

class Test
  extends hammerlab.Suite
     /*with CaseClass */ {
  test("diffs") {
//    !![Cmp[A]]
    CanEq.caseClassBase[Int]
    CanEq.cmpConsAll[String, Int :: HNil, (Int, Int) :: HNil]: Cmp.Aux[String :: Int :: HNil, NeoHList[(String, String) :: (Int, Int) :: HNil]]
    CanEq.cmpCaseClassAll[A, String :: Int :: HNil]
    !![Cmp[D]]

    val Δ = cmp(A("aaa", 111), A("bbb", 222))
    Δ foreach {
      case NeoHList.Cons((ls, rs), NeoHList.Cons((ln, rn), OHList.nil)) ⇒
        println(s"ls: $ls, rs: $rs, ln: $ln, rn: $rn")
    }

//    ===(
//      ,
//      None
//    )
  }
}

object Test {
  case class A(s: String, n: Int)
  case class B(v: Boolean)
  case class C(s: String)
  case class D(a: A, b: B, c: C)
}
