package org.hammerlab.cmp

import hammerlab.cmp.\
import hammerlab.option._
import hammerlab.or._

trait tuples {
  implicit def tuplesCanEq[
    L1, R1,
    L2, R2
  ](
    implicit
    leq: L1 \ L2,
    req: R1 \ R2
  ):
  CanEq.Aux[
    (L1, R1),
    (L2, R2),
    leq.Δ ||
    req.Δ
  ]
  =
    new CanEq[
      (L1, R1),
      (L2, R2)
    ] {
      type Δ = leq.Δ || req.Δ
      def cmp(
        l: (L1, R1),
        r: (L2, R2)
      ): ?[Δ] =
        leq(l._1, r._1) ||
        req(l._2, r._2)
    }
}
