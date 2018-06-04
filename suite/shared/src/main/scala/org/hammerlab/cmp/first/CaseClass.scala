package org.hammerlab.cmp.first

import org.hammerlab.cmp.Priority4CanEq
import org.hammerlab.test.Cmp
import shapeless._

/**
 * Automatic derivations of [[Cmp]] instances for "product" types (case classes)
 *
 * Instances are compared field by field with [[Cmp]] instances corresponding to each field's type, and the first
 * field-difference found is returned.
 *
 * The returned [[org.hammerlab.cmp.CanEq.Diff "diff"]] type is a [[Coproduct]] ("either") of all the fields'
 * diff-types.
 *
 * Mixes-in [[Priority4CanEq]] to have generic-autoderivation take precedence over [[Ordering]]-based derivation, which
 * is relevant for e.g. [[Tuple2]]s where an [[Ordering]] is in scope by default
 */
trait CaseClass
  extends Priority4CanEq {

  /**
   * Derive a [[Cmp]] instance for a case class.
   * @tparam E the [[Coproduct]] "diff"-type returned by [[cmpCons]], which is a disjunction of fields' respective
   *           diff-types
   */
  implicit def cmpCaseClass[T, L <: HList, E <: Coproduct](
    implicit
    gen: Generic.Aux[T, L],
    listCmp: Lazy[Cmp.Aux[L, E]]
  ):
    Cmp.Aux[T, E] =
    Cmp[T, E] {
      (l, r) ⇒
        listCmp
          .value
          .cmp(
            gen.to(l),
            gen.to(r)
          )
    }

  /**
   * Derive a [[Cmp]] instance for an [[HList]] based on [[Cmp]]s for all its fields/elements.
   *
   * Only a representation of the first differing position is returned, and the output / "diff"-type is a
   * [[Coproduct disjunction]] of its elements' diff-types.
   *
   * @param head [[Cmp]] instance for the first element of the [[HList]] [[H :: T]]
   * @param tail [[Cmp]] instance for the tail of the [[HList]] [[H :: T]]
   * @tparam H head-type of the returned [[HList]] [[H :: T]]
   * @tparam T tail-type of the returned [[HList]] [[H :: T]]
   * @tparam DT diff-type for tail-[[Cmp]] instance `tail`
   * @return
   */
  implicit def cmpCons[H, T <: HList, DT <: Coproduct](
    implicit
    head: Lazy[Cmp[H]],
    tail: Lazy[Cmp.Aux[T, DT]]
  ):
    Cmp.Aux[H :: T, head.value.Diff :+: DT] =
    Cmp[H :: T, head.value.Diff :+: DT](
      (l, r) ⇒
        head
          .value
          .cmp(l.head, r.head)
          .map(Inl[head.value.Diff, DT](_))
          .orElse(
            tail
              .value
              .cmp(l.tail, r.tail)
              .map(Inr[head.value.Diff, DT](_))
          )
    )

  /**
   * Work-around for https://github.com/scala/bug/issues/10917
   *
   * Sometimes assertions involving empty collections/tuples are easier if instances involving [[Nothing]] can be
   * derived.
   */
  implicit def consNothing[T <: HList, DT <: Coproduct](
    implicit
    tail: Lazy[Cmp.Aux[T, DT]]
  ):
    Cmp.Aux[Nothing :: T, Nothing :+: DT] =
    Cmp[Nothing :: T, Nothing :+: DT](
      (l, r) ⇒
        the[Cmp[Nothing]].apply(l.head, r.head)
          .map(Inl[Nothing, DT](_))
          .orElse(
            tail
              .value
              .cmp(l.tail, r.tail)
              .map(Inr[Nothing, DT](_))
          )
    )

  implicit val cmpHNil: Cmp.Aux[HNil, CNil] = Cmp[HNil, CNil]((_, _) ⇒ None)
}
