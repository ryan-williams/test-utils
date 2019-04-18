package org.hammerlab.cmp.all

import org.hammerlab.cmp.{ CanEq, Cmp }
import org.hammerlab.cmp.first.SealedTrait
import org.hammerlab.shapeless.neohlist.NeoHList.Cons
import org.hammerlab.shapeless.neohlist.{ NeoHList, OHList }
import shapeless._

/**
 * Automatic derivations of [[Cmp]] instances for "product" types (case classes)
 *
 * Instances are compared field by field with [[Cmp]] instances corresponding to each field's type, and the first
 * field-difference found is returned.
 *
 * The returned [[org.hammerlab.cmp.CanEq.Δ "diff"]] type is a [[Coproduct]] ("either") of all the fields'
 * diff-types.
 */
trait CaseClass2
  extends SealedTrait {

  /**
   * Derive a [[Cmp]] instance for a case class.
   */
  implicit def cmpCaseClassAll[T, L <: HList](
    implicit
    gen: Generic.Aux[T, L],
    cmp: Cmp[L]
  ):
    Cmp.Aux[T, cmp.Δ] =
    Cmp[T, cmp.Δ] {
      (l, r) ⇒
        cmp
          .cmp(
            gen.to(l),
            gen.to(r)
          )
    }

  import OHList.Empty

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
   * @return
   */
  implicit def cmpConsAll[H, T <: HList, Δs <: HList, Δ](
    implicit
    head: Lazy[Cmp[H]],
    tail: Lazy[Cmp.Aux[T, NeoHList[Δ, Δs]]],
    empty: Empty[Δ :: Δs],
  ):
    Cmp.Aux[H :: T, NeoHList[head.value.Δ, Δ :: Δs]] =
    Cmp[H :: T, NeoHList[head.value.Δ, Δ :: Δs]] (
      (l, r) ⇒
        (
          head.value.cmp(l.head, r.head),
          tail.value.cmp(l.tail, r.tail),
        ) match {
          case (Some(d), Some(ds)) ⇒ Some(NeoHList(d, ds))
          case (Some(d), None    ) ⇒ Some(NeoHList(d, empty()))
          case (None   , Some(ds)) ⇒ Some(NeoHList(ds))
          case (None   , None    ) ⇒ None
        }
    )

  /**
   * Work-around for https://github.com/scala/bug/issues/10917
   *
   * Sometimes assertions involving empty collections/tuples are easier if instances involving [[Nothing]] can be
   * derived.
   */
//  implicit def consNothing[T <: HList, DT <: Coproduct](
//    implicit
//    tail: Lazy[Cmp.Aux[T, DT]]
//  ):
//    Cmp.Aux[Nothing :: T, Nothing :+: DT] =
//    Cmp    [Nothing :: T, Nothing :+: DT] { (_, _) ⇒ ??? }

  implicit def cmpHNil[L <: HNil]: Cmp.Aux[L, CNil] = Cmp[L, CNil]((_, _) ⇒ None)
}

trait CaseClass extends CaseClass2 {
  implicit def caseClassBase[T](implicit cmp: Cmp[T]): Cmp.Aux[T :: HNil, NeoHList[cmp.Δ, HNil]] =
    Cmp[T :: HNil, NeoHList[cmp.Δ, HNil]] {
      case (l :: HNil, r :: HNil) ⇒
        cmp(l, r).map {
          NeoHList(_)
        }
    }
}
