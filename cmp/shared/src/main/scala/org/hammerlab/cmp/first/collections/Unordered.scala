package org.hammerlab.cmp.first.collections

import cats.data.Ior._
import hammerlab.cmp.{ ≈, \ }
import hammerlab.option._
import org.hammerlab.cmp.{ CanEq, Cmp }
import org.hammerlab.collection.Iter

trait NothingMap extends Iterables {
  implicit def nothingMapKeys[K, V](implicit cmp: Cmp[Map[K, V]]): CanEq.Aux[Map[K, V], Map[Nothing, Nothing], cmp.Δ] =
    CanEq {
      (l, r) ⇒ cmp(l, r.asInstanceOf[Map[K, V]])
    }

  implicit def nothingNothingMap[Δ]: Cmp.Aux[Map[Nothing, Nothing], Δ] = Cmp { (l, r) ⇒ ??? }
}

/**
 * [[CanEq]] instances for standard collections, computing and returning the first index where they differ, as well as
 * [[ElemDiff a structured representation of that difference]].
 */
trait Unordered
  extends NothingMap {

  def unorderedCmp[T](l: List[T], r: List[T])(implicit ≈ : Cmp[T]): ?[ElemOnly[T, T]] =
    (l, r) match {
      case (Nil, Nil) ⇒ None
      case (Nil, r :: _) ⇒ Some(RightOnly(r))
      case (l :: left, right) ⇒
        {
          for {
            (r, idx) ← right.zipWithIndex
            if ≈(l, r).isEmpty
            rest = right.slice(0, idx) ++ right.drop(idx + 1)
            if unorderedCmp(left, rest).isEmpty
          } yield
            ()
        }
        .headOption
        .fold {
          Option(LeftOnly(l): ElemOnly[T, T])
        } {
          _ ⇒ None
        }
    }

  implicit def setsCanEq[T](
    implicit
    ≈ : ≈[T],
    ord: Ordering[T] = null
  ):
    Cmp.Aux[
      Set[T],
      ElemDiff[T, T, ≈.Δ]
    ] =
    Option(ord) match {
      case Some(_) ⇒
        val ≈* = traversesCanEq(≈, Iter.vector, Iter.vector)
        CanEq {
          (l, r) ⇒
            ≈*(
              l.toVector.sorted,
              r.toVector.sorted,
            )
            .map { _._2 }
        }
      case None ⇒
        CanEq {
          (l, r) ⇒
            unorderedCmp(
              l.toList,
              r.toList
            )(
              ≈
            )
        }
    }

  /**
   * Compare two [[Map]]s; takes precedence over [[traversesCanEq]]
   */
  implicit def mapsCanEq[K, V1, V2](
    implicit
    cmp: V1 \ V2
  ):
    CanEq.Aux[
      Map[K, V1],
      Map[K, V2],
      DiffT[K, V1, V2, cmp.Δ]
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
