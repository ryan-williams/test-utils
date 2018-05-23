package org.hammerlab.cmp

import cats.Eq

/**
 * Type-class representing a comparison between two types, with customizable [[Error]] output-type
 */
trait CanEq[-L, -R] {
  type Error
  def cmp(l: L, r: R): Option[Error]
  def apply(l: L, r: R): Option[Error] = cmp(l, r)
  def eqv(l: L, r: R): Boolean = cmp(l, r).isEmpty
}

trait LowPriCanEq
  extends Serializable {

  type Cmp[T] = CanEq[T, T]
  object Cmp {
    type Aux[T, E] = CanEq.Aux[T, T, E]
    def apply[T, E](fn: (T, T) ⇒ Option[E]): Cmp.Aux[T, E] = CanEq.instance[T, T, E](fn)
    def by[T, U](fn: U ⇒ T)(implicit cmp: Cmp[T]): Cmp.Aux[U, cmp.Error] =
      Cmp {
        (l, r) ⇒
          cmp(
            fn(l),
            fn(r)
          )
      }
  }

  type Aux[T, U, E] = CanEq[T, U] { type Error = E }

  def instance[T, U, E](fn: (T, U) ⇒ Option[E]): CanEq.Aux[T, U, E] =
    new CanEq[T, U] {
      type Error = E
      override def cmp(t: T, u: U): Option[Error] = fn(t, u)
    }

  implicit def fromEq[T](implicit e: Eq[T]): Cmp.Aux[T, (T, T)] =
    instance(
      (t1, t2) ⇒
        if (e.eqv(t1, t2))
          None
        else
          Some(
            (t1, t2)
          )
    )
}

trait MkCanEq
  extends LowPriCanEq {

  def withConversion[T, U](implicit ce: CanEq[T, T], conv: U ⇒ T): CanEq.Aux[T, U, ce.Error] =
    instance((t, u) ⇒ ce.cmp(t, u))
}

object CanEq
  extends MkCanEq
  with first.all {
  def cmp[L, R, E](l: L, r: R)(implicit cmp: CanEq.Aux[L, R, E]): Option[E] = cmp(l, r)
}
