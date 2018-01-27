package org.hammerlab.test

import cats.Eq

trait CanEq[T, U] {
  def eqv(t: T, u: U): Boolean
}

trait LowPri {

  def instance[T, U](implicit fn: (T, U) ⇒ Boolean): CanEq[T, U] =
    new CanEq[T, U] {
      override def eqv(t: T, u: U) = fn(t, u)
    }

  implicit def seqs[T, U](implicit
                          ce: CanEq[T, U]): CanEq[Seq[T], Seq[U]] = {
    val iters = iterators[T, U]
    instance[Seq[T], Seq[U]](
      (s1, s2) ⇒
        iters.eqv(
          s1.iterator,
          s2.iterator
        )
    )
  }

  def iterators[T, U](implicit ce: CanEq[T, U]): CanEq[Iterator[T], Iterator[U]] =
    new CanEq[Iterator[T], Iterator[U]] {
      override def eqv(t: Iterator[T], u: Iterator[U]) =
        (t.hasNext, u.hasNext) match {
          case (true, true) ⇒ ce.eqv(t.next, u.next) && eqv(t, u)
          case (false, false) ⇒ true
          case _ ⇒ false
        }
    }
}

object CanEq extends LowPri {

  implicit def fromEq[T](implicit e: Eq[T]): CanEq[T, T] = instance(e.eqv)

  def fromExisting[T, U](implicit ce: CanEq[T, T], conv: U ⇒ T): CanEq[T, U] =
    instance((t, u) ⇒ ce.eqv(t, u))
}
