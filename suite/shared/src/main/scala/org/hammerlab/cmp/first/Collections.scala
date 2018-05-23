package org.hammerlab.cmp.first

import cats.data.Ior._
import org.hammerlab.cmp.CanEq
import org.hammerlab.cmp.CanEq.instance

/**
 * [[CanEq]] instances for standard collections, computing and returning the [[hammerlab.cmp.first first]] index where
 * they differ, as well as a structured representation of that difference (see
 * [[hammerlab.cmp.first.ElemDiff ElemDiff]]).
 */
trait Collections {

  /**
   * Sum-type for the first difference found between two collections
   *
   * @tparam L type of elements in the LHS collection; returned as a [[LeftOnly]] if e.g. the RHS is a prefix of the LHS
   *           (so the first differing index is just beyond the length of the RHS)
   * @tparam R type of elements in the RHS collection; returned as a [[RightOnly]] if e.g. the LHS is a prefix of the
   *           RHS (so the first differing index is just beyond the length of the LHS)
   * @tparam B error-type returned by a [[CanEq]] that compares an [[L]] and [[R]] and finds a difference before the end
   *           of either collection; this gets wrapped in and returned as a [[Diff]]
   */
  sealed trait  ElemDiff[+L, +R, +B]       extends Product with Serializable
  case class  LeftOnly[ L,  R,  B](l: L) extends ElemDiff[L, R, B]
  case class RightOnly[ L,  R,  B](r: R) extends ElemDiff[L, R, B]
  case class      Diff[ L,  R,  B](b: B) extends ElemDiff[L, R, B]


  /**
   * Error-type for a [[CanEq]] that returns an index/"key" [[K]] at which two collections have a different value
   * (expressed as an [[ElemDiff]]
   */
  type ErrT[K, L, R, E] = (K, ElemDiff[L, R, E])

  /**
   * Collection-error-type specialized for [[Int]]-indices
   */
  type IdxError[L, R, E] = ErrT[Int, L, R, E]

  implicit def arraysCanEq[T, U](
    implicit
    ce: CanEq[T, U]
  ):
    CanEq.Aux[
      Array[T],
      Array[U],
      IdxError[T, U, ce.Error]
    ] =
    instance {
      (s1, s2) ⇒
        iteratorsCanEq(ce)(
          s1.iterator,
          s2.iterator
        )
    }

  implicit def seqsCanEq[T, U](
    implicit
    ce: CanEq[T, U]
  ):
    CanEq.Aux[
      Seq[T],
      Seq[U],
      IdxError[T, U, ce.Error]
    ] =
    instance {
      (s1, s2) ⇒
        iteratorsCanEq(ce)(
          s1.iterator,
          s2.iterator
        )
    }

  implicit def mapsCanEq[K, V1, V2](
    implicit
    cmp: CanEq[V1, V2]
  ):
    CanEq.Aux[
      Map[K, V1],
      Map[K, V2],
      ErrT[K, V1, V2, cmp.Error]
    ] =
    instance {
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

  implicit def iteratorsCanEq[T, U](
    implicit
    ce: CanEq[T, U]
  ):
    CanEq.Aux[
      Iterator[T],
      Iterator[U],
      IdxError[T, U, ce.Error]
    ] =
    new CanEq[Iterator[T], Iterator[U]] {
      type Error = IdxError[T, U, ce.Error]
      def cmp(          t: Iterator[T], u: Iterator[U]): Option[Error] = cmp(0, t, u)
      def cmp(idx: Int, t: Iterator[T], u: Iterator[U]): Option[Error] =
        (t.hasNext, u.hasNext) match {
          case (true, true) ⇒
            ce(
              t.next,
              u.next
            )
            .map(e ⇒ idx → Diff(e))
            .orElse(
              cmp(
                idx + 1,
                t,
                u
              )
            )
          case ( true, false) ⇒ Some(idx →  LeftOnly(t.next))
          case (false,  true) ⇒ Some(idx → RightOnly(u.next))
          case _ ⇒ None
        }
    }
}
