package org.hammerlab.cmp.all

import org.hammerlab.cmp.all.Test._
import org.hammerlab.cmp.{ CanEq, Cmp }
import org.hammerlab.shapeless.neohlist.{ NeoHList, OHList }
import shapeless._
import NeoHList.{ Cons, Extend }

class Test
  extends hammerlab.Suite {
  test("diffs") {
//    !![Cmp[A]]
    CanEq.caseClassBase[Int]
    CanEq.cmpConsAll[String, Int :: HNil, HNil, (Int, Int)]: Cmp.Aux[String :: Int :: HNil, NeoHList[(String, String), (Int, Int) :: HNil]]
    CanEq.cmpCaseClassAll[A, String :: Int :: HNil]
    !![Cmp[D]]

//    !![Generic[OHList.Cons[String, HNil]]]
//    !![Generic[NeoHList.Cons[String, HNil]]]

//    !![Generic[OHList[HNil]]]
//    !![Generic[OHList[String :: HNil]]]

    //!![Generic[NeoHList[HNil]]]
    !![Generic[NeoHList[String, HNil]]]

    !![Generic[NeoHList.Extend[String, Int, HNil]]]
    !![Generic[NeoHList[String, Int :: HNil]]]

    !![Generic[NeoHList[(String, String), (Int, Int) :: HNil]]]

    !![
      Generic.Aux[
        NeoHList[
          (String, String),
          (Int, Int) :: HNil
        ],
        Cons[(String, String), (Int, Int) :: HNil] :+:
        Extend[(String, String), (Int, Int), HNil] :+:
        CNil
      ]
    ]

    //    !![Generic[NeoHList.Extend[String :: HNil]]]

    //!![Cmp[OHList.Cons[String, HNil]]]
    //!![Cmp[OHList[String :: HNil]]]
    //!![Cmp[NeoHList[(String, String) :: HNil]]]

    val Δ = cmp(A("aaa", 111), A("bbb", 222))
    Δ foreach {
      case NeoHList.Cons((ls, rs), NeoHList.Cons((ln, rn), OHList.nil)) ⇒
        println(s"ls: $ls, rs: $rs, ln: $ln, rn: $rn")
    }

    !![Generic    [OHList.nil.type      ]]
    !![Generic.Aux[OHList.nil.type, HNil]]

    ==(HNil, HNil)

    ==(OHList.nil, OHList.nil)

    !![Generic[OHList[HNil]]]
    !![Generic.Aux[OHList[HNil], HNil]]

    !![Cmp[HNil]]
    !![Cmp.Aux[HNil, CNil]]

    !![Cmp    [OHList[HNil]]]
    CanEq.cmpCaseClassAll[OHList[HNil], HNil]: Cmp.Aux[OHList[HNil], CNil]
    !![Cmp.Aux[OHList[HNil], CNil]]

    !![Generic    [Cons[Int, HNil]]]
    !![Generic.Aux[Cons[Int, HNil], Int :: OHList[HNil] :: HNil]]
    !![Cmp[Int]]
    !![Cmp[OHList[HNil]]]
    !![Cmp[HNil]]
    !![Cmp[Int :: HNil]]
    !![Cmp[OHList[HNil] :: HNil]]
    !![Cmp[OHList[HNil] :: Int :: HNil]]

    CanEq.cmpConsAll[Int, OHList[HNil] :: HNil, HNil, CNil]
    !![Cmp[Int :: OHList[HNil] :: HNil]]

    !![Cmp[Cons[Int, HNil]]]

    ==(
      Cons((111, 222), OHList.nil),
      Cons((111, 222), OHList.nil)
    )

    !=(
      Cons((111, 222), OHList.nil),
      Cons((111, 333), OHList.nil)
    )

    ==(
      Cons(("aaa", "bbb"), Cons((111, 222), OHList.nil)),
      Cons(("aaa", "bbb"), Cons((111, 222), OHList.nil))
    )

    !=(
      Cons(("aaa", "bbb"), Cons((111, 222), OHList.nil)),
      Cons(("aaa", "ccc"), Cons((111, 222), OHList.nil))
    )


    !=(
      Cons(("aaa", "bbb"), Cons((111, 222), OHList.nil)),
      Cons(("aaa", "bbb"), Cons((111, 333), OHList.nil))
    )

    !=(
      Cons(("aaa", "bbb"), Cons((111, 222), OHList.nil)),
      Cons(("aaa", "ccc"), Cons((111, 333), OHList.nil))
    )

    ==(
      cmp(111, 111),
      None
    )

    ==(
      cmp(111, 222),
      Some((111, 222))
    )

    ==(
      cmp("aaa", "aaa"),
      None
    )

    ==(
      cmp("aaa", "bbb"),
      Some(("aaa", "bbb"))
    )

    ==(cmp(HNil, HNil), None)

    !![Cmp    [Int :: HNil]]
    //!![Cmp.Aux[Int :: HNil, Cons[(Int, Int), HNil]]]
    !![Cmp.Aux[Int :: HNil, NeoHList[(Int, Int), HNil]]]

    !![Cmp[Cons[(Int, Int), HNil]]]

    !![Generic[NeoHList[(Int, Int), HNil]]]
    !![Generic.Aux[NeoHList[(Int, Int), HNil], Cons[(Int, Int), HNil] :+: CNil]]

    //CanEq.cmpCaseClassAll[]
    !![Cmp[NeoHList[(Int, Int), HNil]]]

    ==(cmp(111 :: HNil, 111 :: HNil), None)
    ==(cmp(111 :: HNil, 222 :: HNil), Some(Cons((111, 222))))

    ==(cmp("aaa" :: 111 :: HNil, "aaa" :: 111 :: HNil), None)
    ==(cmp("aaa" :: 111 :: HNil, "aaa" :: 222 :: HNil))(Some(Extend(Cons((111, 222)))))
    ==(cmp("aaa" :: 111 :: HNil, "bbb" :: 111 :: HNil))(Some(NeoHList(("aaa", "bbb"), OHList.empty[(Int, Int) :: HNil])))
    ==(cmp("aaa" :: 111 :: HNil, "bbb" :: 222 :: HNil))(Some(Cons(("aaa", "bbb"), Cons((111, 222)))))

    ==(cmp(A("aaa", 111), A("aaa", 111)), None)
    ==(cmp(A("aaa", 111), A("aaa", 222)))(Some(Extend(Cons((111, 222)))))
    ==(cmp(A("aaa", 111), A("bbb", 111)))(Some(NeoHList(("aaa", "bbb"), OHList.empty[(Int, Int) :: HNil])))
    ==(cmp(A("aaa", 111), A("bbb", 222)))(Some(Cons(("aaa", "bbb"), Cons((111, 222)))))

    //    ==(
//      cmp(A("aaa", 111), A("aaa", 111)),
//      None
//    )

//    ==(
//      cmp(A("aaa", 111), A("aaa", 222)),
//      Some(Extend[(String, String), (Int, Int), HNil](Cons((111, 222))))
//    )

    //    ==(
//      Δ.get,
//      Cons(("aaa", "bbb"), Cons((111, 222), OHList.nil))
//    )

//    ==(
//      Δ,
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
