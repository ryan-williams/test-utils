package org.hammerlab.cmp

import cats.Eq

/**
 * Type-class for comparing two types, with customizable `Error` output-type containing a possibly-structured
 * representation of the "diff", if any
 */
trait CanEq[-L, -R] {
  type Error
  def   cmp(l: L, r: R): Option[Error]
  def apply(l: L, r: R): Option[Error] = cmp(l, r)
  def   eqv(l: L, r: R): Boolean = cmp(l, r).isEmpty
}

trait LowPriCanEq
  extends Serializable {

  /** Short-hand for a [[CanEq]] whose comparee-types are equal */
  type Cmp[T] = CanEq[T, T]
  object Cmp {
    type Aux[T, E] = CanEq.Aux[T, T, E]

    /** Create a [[Cmp]] from its single method */
    def apply[T, E](fn: (T, T) ⇒ Option[E]): Aux[T, E] = CanEq(fn)

    /** Create a [[Cmp]] interms of another */
    def by[T, U](fn: U ⇒ T)(implicit cmp: Cmp[T]): Aux[U, cmp.Error] =
      Cmp {
        (l, r) ⇒
          cmp(
            fn(l),
            fn(r)
          )
      }
  }

  type Aux[T, U, E] = CanEq[T, U] { type Error = E }

  /**
   * Short-hand for creating [[CanEq]] instances from a single method.
   *
   * TODO: can probably go away in favor of SAM-syntax once Scala 2.11 support is dropped.
   */
  def apply[T, U, E](fn: (T, U) ⇒ Option[E]): Aux[T, U, E] =
    new CanEq[T, U] {
      type Error = E
      override def cmp(t: T, u: U): Option[Error] = fn(t, u)
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
     with first.all {

  /**
   * Create a [[CanEq]] for two different types given a [[Cmp]] for one and a conversion function for the other into the
   * [[Cmp]]'s type
   */
  def withConversion[T, U](
    implicit
    cmp: Cmp[T],
    conv: U ⇒ T
  ):
    CanEq.Aux[T, U, cmp.Error] =
    CanEq(cmp(_, _))

  trait dsl {
    /** Short-hand for applying a [[CanEq]] to two objects and returning the "error", if any */
    def cmp[L, R](l: L, r: R)(implicit cmp: CanEq[L, R]): Option[cmp.Error] = cmp(l, r)
  }

  object dsl extends dsl
}
