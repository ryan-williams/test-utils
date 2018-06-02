package org.hammerlab.cmp.first

import cats.data.Ior._
import org.hammerlab.cmp.CanEq
import Collections._

object Collections {
  /**
   * Sum-type for the first difference found between two collections
   *
   * @tparam L type of elements in the LHS collection; returned as a [[LeftOnly]] if e.g. the RHS is a prefix of the LHS
   *           (so the first differing index is just beyond the length of the RHS)
   * @tparam R type of elements in the RHS collection; returned as a [[RightOnly]] if e.g. the LHS is a prefix of the
   *           RHS (so the first differing index is just beyond the length of the LHS)
   * @tparam B "both": diff-type returned by a [[CanEq]] that compares an [[L]] and [[R]] and finds a difference before
   *           the end of either collection; this gets wrapped in and returned as a [[Diff]]
   */
  sealed trait  ElemDiff[+L, +R, +B]       extends Product with Serializable
    case class  LeftOnly[ L,  R,  B](l: L) extends ElemDiff[L, R, B]
    case class RightOnly[ L,  R,  B](r: R) extends ElemDiff[L, R, B]
    case class      Diff[ L,  R,  B](b: B) extends ElemDiff[L, R, B]

  /**
   * Diff-type for a [[CanEq]] that returns an index/"key" [[Key]] at which two collections have a different value
   * (expressed as an [[ElemDiff]]
   */
  private type DiffT[Key, Left, Right, Diff] = (Key, ElemDiff[Left, Right, Diff])

  /**
   * Collection-diff-type specialized for [[Int]]-indices
   */
  type IndexedDiff[Left, Right, Diff] = DiffT[Int, Left, Right, Diff]
}

trait LowPriorityCollections
  extends SealedTrait {

  implicit def iterablesCanEq[L, R](
    implicit
    ce: CanEq[L, R]
  ):
  CanEq.Aux[
    Iterable[L],
    Iterable[R],
    IndexedDiff[L, R, ce.Diff]
  ] =
    CanEq {
      (s1, s2) ⇒
        iteratorsCanEq(ce)(
          s1.iterator,
          s2.iterator
        )
    }

  implicit def iteratorsCanEq[L, R](
    implicit
    ce: CanEq[L, R]
  ):
    CanEq.Aux[
      Iterator[L],
      Iterator[R],
      IndexedDiff[L, R, ce.Diff]
    ] =
    new CanEq[Iterator[L], Iterator[R]] {
      type Diff = IndexedDiff[L, R, ce.Diff]
      def cmp(          l: Iterator[L], r: Iterator[R]): Option[Diff] = cmp(0, l, r)
      def cmp(idx: Int, l: Iterator[L], r: Iterator[R]): Option[Diff] =
        (l.hasNext, r.hasNext) match {
          case (true, true) ⇒
            ce(
              l.next,
              r.next
            )
            .map(e ⇒ idx → Diff(e))
            .orElse(
              cmp(
                idx + 1,
                l,
                r
              )
            )
          case ( true, false) ⇒ Some(idx →  LeftOnly(l.next))
          case (false,  true) ⇒ Some(idx → RightOnly(r.next))
          case _ ⇒ None
        }
    }
}

/**
 * [[CanEq]] instances for standard collections, computing and returning the first index where they differ, as well as
 * [[ElemDiff a structured representation of that difference]].
 */
trait Collections
  extends LowPriorityCollections {

  implicit def arraysCanEq[T, U](
    implicit
    ce: CanEq[T, U]
  ):
    CanEq.Aux[
      Array[T],
      Array[U],
      IndexedDiff[T, U, ce.Diff]
    ] =
    CanEq {
      (s1, s2) ⇒
        iteratorsCanEq(ce)(
          s1.iterator,
          s2.iterator
        )
    }

  /**
   * Compare two [[Map]]s; takes precedence over [[iterablesCanEq]]
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
