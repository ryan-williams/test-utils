package org.hammerlab.cmp.first

import cats.data.Ior._
import org.hammerlab.cmp.first.Collections._
import org.hammerlab.cmp.{ CanEq, Priority2CanEq }
import org.hammerlab.test.Cmp
import shapeless.Lazy

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
  sealed trait  ElemOnly[+L, +R    ]       extends ElemDiff[L, R, Nothing]
    case class  LeftOnly[ L        ](l: L) extends ElemOnly[L, Nothing]
    case class RightOnly[     R    ](r: R) extends ElemOnly[Nothing, R]
    case class      Diff[         B](b: B) extends ElemDiff[Nothing, Nothing, B]

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

trait CanIterate[T[_]] {
  def apply[A](t: T[A]): Iterator[A]
}
object CanIterate {
  implicit val array: CanIterate[Array] =
    new CanIterate[Array] {
      def apply[A](t: Array[A]) = t.toIterator
    }
  implicit def option[Opt[T] <: Option[T]]: CanIterate[Opt] =
    new CanIterate[Opt] {
      def apply[A](t: Opt[A]) = t.toIterator
    }
  implicit val iterator: CanIterate[Iterator] =
    new CanIterate[Iterator] {
      def apply[A](t: Iterator[A]) = t
    }
  implicit def traversable[I[T] <: scala.collection.Traversable[T]]: CanIterate[I] =
    new CanIterate[I] {
      def apply[A](t: I[A]) = t.toIterator
    }
}

trait LowPriorityCollections
  extends SealedTrait
     with CaseClass
     with Priority2CanEq {

  implicit def cmpRange(
    implicit
    cmp: Lazy[Cmp[Int]]
  ):
    Cmp.Aux[
      Range,
      IndexedDiff[
        Int,
        Int,
        cmp.value.Diff
      ]
    ] =
    Cmp.by(
      r ⇒ r: Seq[Int]
    )

  implicit def traversesCanEq[L, R, T[_]](
    implicit
    cmp: Lazy[CanEq[L, R]],
    t: CanIterate[T]
  ):
    CanEq.Aux[
      T[L],
      T[R],
      IndexedDiff[L, R, cmp.value.Diff]
    ] =
    CanEq {
      (l, r) ⇒
        iteratorsCanEq(cmp)(
          t(l),
          t(r)
        )
    }

  /**
   * Work-around for https://github.com/scala/bug/issues/10917
   *
   * Sometimes asserting things on an empty collection is easier if instances involving [[Nothing]] can be derived.
   */
  implicit def traverseNothingsCanEq[T[_]](
    implicit
    t: CanIterate[T]
  ):
    Cmp.Aux[
      T[Nothing],
      IndexedDiff[Nothing, Nothing, Nothing]
    ] =
    CanEq {
      (l, r) ⇒
        iteratorsCanEq[Nothing, Nothing].apply(
          t(l),
          t(r)
        )
    }

  def iteratorsCanEq[L, R](
    implicit
    ce: Lazy[CanEq[L, R]]
  ):
    CanEq.Aux[
      Iterator[L],
      Iterator[R],
      IndexedDiff[L, R, ce.value.Diff]
    ] =
    new CanEq[Iterator[L], Iterator[R]] {
      type Diff = IndexedDiff[L, R, ce.value.Diff]
      def cmp(          l: Iterator[L], r: Iterator[R]): Option[Diff] = cmp(0, l, r)
      def cmp(idx: Int, l: Iterator[L], r: Iterator[R]): Option[Diff] =
        (l.hasNext, r.hasNext) match {
          case (true, true) ⇒
            ce.value.apply(
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
