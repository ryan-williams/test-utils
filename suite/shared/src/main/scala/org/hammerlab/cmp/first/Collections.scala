package org.hammerlab.cmp.first

import cats.data.Ior
import cats.data.Ior._
import org.hammerlab.cmp.CanEq
import org.hammerlab.cmp.CanEq.instance
import shapeless._

trait Collections {

  implicit def arrs[T, U](
    implicit
    ce: CanEq[T, U]
  ):
    CanEq.Aux[
      Array[T],
      Array[U],
      (
        Int,
        Option[ce.Error]
      )
    ] =
    instance {
      (s1, s2) ⇒
        iterators(ce)(
          s1.iterator,
          s2.iterator
        )
    }

  implicit def seqs[T, U](
    implicit
    ce: CanEq[T, U]
  ):
    CanEq.Aux[
      Seq[T],
      Seq[U],
      (
        Int,
        Option[ce.Error]
      )
    ] =
    instance {
      (s1, s2) ⇒
        iterators(ce)(
          s1.iterator,
          s2.iterator
        )
    }

  implicit def maps[K, V1, V2](
    implicit
    cmp: CanEq[V1, V2]
  ):
    CanEq.Aux[
      Map[K, V1],
      Map[K, V2],
      (
        K,
        V1 :+: V2 :+: cmp.Error :+: CNil
      )
    ] =
    instance {
      (m1, m2) ⇒
        val it =
          for {
             k ← (m1.keySet ++ m2.keySet).iterator
            v1 = m1.get(k)
            v2 = m2.get(k)
            vs ← fromOptions(v1, v2)
             e ← (
                   vs match {
                     case  Left(v) ⇒ Some(    Inl(v) )
                     case Right(v) ⇒ Some(Inr(Inl(v)))
                     case Both(v1, v2) ⇒
                       cmp(v1, v2)
                         .map {
                           e ⇒
                             Inr(Inr(Inl(e)))
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

  implicit def iterators[T, U](
    implicit
    ce: CanEq[T, U]
  ):
    CanEq.Aux[
      Iterator[T],
      Iterator[U],
      (
        Int,
        Option[ce.Error]
      )
    ] =
    new CanEq[Iterator[T], Iterator[U]] {
      type Error = (Int, Option[ce.Error])
      override def cmp(t: Iterator[T], u: Iterator[U]): Option[Error] = cmp(0, t, u)
      def cmp(idx: Int, t: Iterator[T], u: Iterator[U]): Option[Error] =
        (t.hasNext, u.hasNext) match {
          case (true, true) ⇒
            ce(
              t.next,
              u.next
            )
            .map(e ⇒ idx → Some(e))
            .orElse(
              cmp(
                idx + 1,
                t,
                u
              )
            )
          case (false, false) ⇒ None
          case _ ⇒ Some((idx, None))
        }
    }
}
