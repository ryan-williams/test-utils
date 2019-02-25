package org.hammerlab.cmp

import cats.Eq

trait CatsEq
  extends Casts {

  /**
   * Derive a [[Cmp]] from an [[Eq]], where the returned "diff"-representation is just a [[Tuple2]] with the two
   * non-equal values
   */
  implicit def fromEq[T](implicit e: Eq[T]): Cmp.Aux[T, (T, T)] =
    CanEq {
      (t1, t2) ⇒
        if (e.eqv(t1, t2))
          None
        else
          Some(
            (t1, t2)
          )
    }
}
