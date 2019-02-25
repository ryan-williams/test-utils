package org.hammerlab.cmp.first.collections

import cats.data.Ior._
import org.hammerlab.cmp.{ CanEq, Cmp }

/**
 * [[CanEq]] instances for standard collections, computing and returning the first index where they differ, as well as
 * [[ElemDiff a structured representation of that difference]].
 */
trait Unordered
  extends Iterables {

  implicit def setsCanEq[T](
    implicit
    cmp: Cmp[T]
  ):
    Cmp.Aux[
      Set[T],
      ElemOnly[T, T]
    ] =
    Cmp {
      (l, r) ⇒
        val it =
          for {
            e ← l.iterator
            if !r(e)
          } yield
            e

        if (it.hasNext)
          Some(LeftOnly(it.next))
        else {
          val it =
            for {
              e ← r.iterator
              if !l(e)
            } yield
              e

          if (it.hasNext)
            Some(RightOnly(it.next))
          else
            None
        }
    }

  /**
   * Compare two [[Map]]s; takes precedence over [[traversesCanEq]]
   */
  implicit def mapsCanEq[K, V1, V2](
    implicit
    cmp: CanEq[V1, V2]
  ):
    CanEq.Aux[
      Map[K, V1],
      Map[K, V2],
      DiffT[K, V1, V2, cmp.Diff]
    ] =
    CanEq {
      (m1, m2) ⇒
        val it =
          for {
             k ← (m1.keySet ++ m2.keySet).iterator
            v1 = m1.get(k)
            v2 = m2.get(k)
            vs ← fromOptions(v1, v2)
             e ←
               (
                 vs match {
                   case  Left(v) ⇒ Some( LeftOnly(v))
                   case Right(v) ⇒ Some(RightOnly(v))
                   case Both(v1, v2) ⇒
                     cmp(v1, v2)
                       .map {
                         e ⇒
                           Diff(e)
                       }
                 }
               )
               .map {
                 (k, _)
               }
          } yield
            e

        if (it.hasNext)
          Some(it.next)
        else
          None
    }
}
