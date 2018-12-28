package org.hammerlab.cmp

import cats.data.Ior

trait tuples {
  implicit def tuplesCanEq[
  L1, R1,
  L2, R2
  ](
       implicit
       leq: CanEq[L1, L2],
       req: CanEq[R1, R2]
   ):
  CanEq.Aux[
    (L1, R1),
    (L2, R2),
    Ior[
      leq.Diff,
      req.Diff
    ]
  ]
  =
    new CanEq[(L1, R1), (L2, R2)] {
      override type Diff = Ior[leq.Diff, req.Diff]
      override def cmp(l: (L1, R1), r: (L2, R2)): Option[Diff] =
        Ior.fromOptions(
          leq(l._1, r._1),
          req(l._2, r._2)
        )
    }
}
