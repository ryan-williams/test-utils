package org.hammerlab.cmp.first.collections

import org.hammerlab.cmp.{ CanEq, Cmp }
import org.hammerlab.cmp.first.CaseClass
import shapeless.Lazy

trait Defaults
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
