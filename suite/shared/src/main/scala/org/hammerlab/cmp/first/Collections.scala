package org.hammerlab.cmp.first

import org.hammerlab.cmp.CanEq
import org.hammerlab.cmp.CanEq.instance

trait Collections {

  implicit def seqs[T, U](implicit
                          ce: CanEq[T, U]): CanEq.Aux[Seq[T], Seq[U], (Int, Option[ce.Error])] = {
    val iters: CanEq.Aux[Iterator[T], Iterator[U], (Int, Option[ce.Error])] = iterators[T, U](ce)
    instance[Seq[T], Seq[U], (Int, Option[ce.Error])](
      (s1, s2) ⇒
        iters(
          s1.iterator,
          s2.iterator
        )
    )
  }

  def iterators[T, U](implicit
                      ce: CanEq[T, U]): CanEq.Aux[Iterator[T], Iterator[U], (Int, Option[ce.Error])] =
    new CanEq[Iterator[T], Iterator[U]] {
      type Error = (Int, Option[ce.Error])
      override def cmp(t: Iterator[T], u: Iterator[U]): Option[Error] = cmp(0, t, u)
      def cmp(idx: Int, t: Iterator[T], u: Iterator[U]): Option[Error] =
        (t.hasNext, u.hasNext) match {
          case (true, true) ⇒
            ce
              .cmp(t.next, u.next)
              .map(e ⇒ idx → Some(e))
              .orElse(
                cmp(
                  idx + 1,
                  t,
                  u
                )
              )
          case (false, false) ⇒ None
          case _ ⇒ Some((idx, None))
        }
    }
}
