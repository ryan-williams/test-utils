package org.hammerlab.test.matchers.seqs

import org.scalatest.matchers.{ MatchResult, Matcher }

case class ArrMatcher[T](expected: Iterable[T],
                         matchOrder: Boolean = true)
  extends Matcher[Array[T]] {
  override def apply(actual: Array[T]): MatchResult =
    SeqMatcher[T](expected, matchOrder)
      .apply(actual)
}

trait HasArrMatcher {
  def arrMatch[T](expected: Iterable[T]): Matcher[Array[T]] = ArrMatcher[T](expected.toSeq)
  def arrMatch[T](expected: Iterator[T]): Matcher[Array[T]] = ArrMatcher[T](expected.toSeq)
}

