package org.hammerlab.cmp

trait utils {
  def ===[T, U](t1: T, t2: U)(implicit cmp: CanEq[T, U]): Option[cmp.Error] =
    cmp(t1, t2)
}

object utils extends utils
