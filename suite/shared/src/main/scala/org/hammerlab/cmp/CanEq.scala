package org.hammerlab.cmp

import cats.Eq
import org.hammerlab.cmp.first.{ CaseClass, Collections }

/**
 * Type-class for comparing instances of two (possibly different )types, with a customizable "diff" output-type
 * containing a configurable representation of the "diff", if any
 */
trait CanEq[-L, -R] {
  type Diff
  def   cmp(l: L, r: R): Option[Diff]
  def apply(l: L, r: R): Option[Diff] = cmp(l, r)
  def   eqv(l: L, r: R):      Boolean = cmp(l, r).isEmpty
}

trait LowPriCanEq
  extends CaseClass
     with Serializable {

  /** Short-hand for a [[CanEq]] whose comparee-types are equal */
  type Cmp[-T] = CanEq[T, T]
  object Cmp {
    type Aux[-T, E] = CanEq.Aux[T, T, E]

    /** Create a [[Cmp]] from its single method */
    def apply[T, E](fn: (T, T) ⇒ Option[E]): Aux[T, E] = CanEq(fn)

    /** Create a [[Cmp]] interms of another */
    def by[T, U](fn: U ⇒ T)(implicit cmp: Cmp[T]): Aux[U, cmp.Diff] =
      Cmp {
        (l, r) ⇒
          cmp(
            fn(l),
            fn(r)
          )
      }
  }

  type Aux[-T, -U, E] = CanEq[T, U] { type Diff = E }

  /**
   * Short-hand for creating [[CanEq]] instances from a single method.
   *
   * TODO: can probably go away in favor of SAM-syntax once Scala 2.11 support is dropped.
   */
  def apply[T, U, D](fn: (T, U) ⇒ Option[D]): Aux[T, U, D] =
    new CanEq[T, U] {
      type Diff = D
      override def cmp(t: T, u: U): Option[Diff] =
        fn(t, u)
    }

  /**
   * Derive a [[Cmp]] from an [[Eq]], where the returned "diff"-representation is just a [[Tuple2]] with the two
   * comparees
   */
  implicit def fromEq[T](implicit e: Eq[T]): Cmp.Aux[T, (T, T)] =
    apply(
      (t1, t2) ⇒
        if (e.eqv(t1, t2))
          None
        else
          Some(
            (t1, t2)
          )
    )
}

object CanEq
  extends LowPriCanEq
     with Collections {

  /**
   * Create a [[CanEq]] for two different types given a [[Cmp]] for one and a conversion function for the other into the
   * [[Cmp]]'s type
   */
  def withConversion[T, U](
    implicit
    cmp: Cmp[T],
    conv: U ⇒ T
  ):
    CanEq.Aux[T, U, cmp.Diff] =
    CanEq(cmp(_, _))

  trait dsl {
    /** Short-hand for applying a [[CanEq]] to two objects and returning the [[CanEq.Diff diff]], if any */
    def cmp[L, R](l: L, r: R)(implicit cmp: CanEq[L, R]): Option[cmp.Diff] =
      cmp(l, r)
  }

  object dsl extends dsl
}
