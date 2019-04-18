package org.hammerlab.cmp.first.collections

import hammerlab.cmp.\
import hammerlab.option._
import org.hammerlab.cmp.{ CanEq, Cmp }
import org.hammerlab.cmp.all.CaseClass
import org.hammerlab.collection.Iter
import shapeless.Lazy

trait Iterables
  extends CaseClass {

  implicit def cmpRange(
    implicit
    cmp: Lazy[Cmp[Int]]
  ):
    Cmp.Aux[
      Range,
      IndexedDiff[
        Int,
        Int,
        cmp.value.Δ
      ]
    ] =
    Cmp.by(
      r ⇒ r: Seq[Int]
    )

  def traversesCanEq[L, R, LI[_], RI[_]](
    implicit
    cmp: L \ R,
    li: Iter[LI],
    ri: Iter[RI],
  ):
    CanEq.Aux[
      LI[L],
      RI[R],
      IndexedDiff[L, R, cmp.Δ]
    ] =
    CanEq {
      (l, r) ⇒
        iteratorsCanEq(cmp)(
          li.iter(l),
          ri.iter(r)
        )
    }

  implicit def traversesCanEqLazy[L, R, LI[_], RI[_]](
    implicit
    cmp: Lazy[L \ R],
    li: Iter[LI],
    ri: Iter[RI],
  ):
    CanEq.Aux[
      LI[L],
      RI[R],
      IndexedDiff[L, R, cmp.value.Δ]
    ] = traversesCanEq(cmp.value, li, ri)

  /**
   * Work-around for https://github.com/scala/bug/issues/10917
   *
   * Sometimes asserting things on an empty collection is easier if instances involving [[Nothing]] can be derived.
   */
  implicit def traverseNothingsCanEq[T[_]](
    implicit
    t: Iter[T]
  ):
    Cmp.Aux[
      T[Nothing],
      IndexedDiff[Nothing, Nothing, Nothing]
    ] =
    CanEq {
      (l, r) ⇒
        iteratorsCanEq[Nothing, Nothing].apply(
          t.iter(l),
          t.iter(r)
        )
    }

  /**
   * This powers comparison for any [[Iter]] types.
   *
   * It isn't implicit; the actual implicit [[Iterator]] instance goes via [[Iter]].
   */
  def iteratorsCanEq[L, R](
    implicit
    ce: L \ R
  ):
    CanEq.Aux[
      Iterator[L],
      Iterator[R],
      IndexedDiff[L, R, ce.Δ]
    ] =
    new CanEq[Iterator[L], Iterator[R]] {
      type Δ = IndexedDiff[L, R, ce.Δ]
      def cmp(          l: Iterator[L], r: Iterator[R]): ?[Δ] = cmp(0, l, r)
      def cmp(idx: Int, l: Iterator[L], r: Iterator[R]): ?[Δ] =
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
