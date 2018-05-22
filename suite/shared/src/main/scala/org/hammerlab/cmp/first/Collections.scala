package org.hammerlab.cmp.first

import cats.data.Ior._
import org.hammerlab.cmp.CanEq
import org.hammerlab.cmp.CanEq.instance

trait Collections {

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

  sealed trait ElemDiff[+L, +R, +B] extends Product with Serializable
  case class  LeftOnly[L, R, B](l: L) extends ElemDiff[L, R, B]
  case class RightOnly[L, R, B](r: R) extends ElemDiff[L, R, B]
  case class      Diff[L, R, B](b: B) extends ElemDiff[L, R, B]

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

  type ErrT[K, L, R, E] = (K, ElemDiff[L, R, E])
  type IdxError[L, R, E] = ErrT[Int, L, R, E]
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
