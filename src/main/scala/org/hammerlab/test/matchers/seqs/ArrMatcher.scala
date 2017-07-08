package org.hammerlab.test.matchers.seqs

import org.scalatest.matchers.{ MatchResult, Matcher }

case class ArrMatcher[T](expected: Seq[T],
                         matchOrder: Boolean = true)
  extends Matcher[Array[T]] {
  override def apply(actual: Array[T]): MatchResult =
    SeqMatcher[T](expected, matchOrder)
      .apply(actual)
}

object ArrMatcher {
  def arrMatch[T](expected: Iterable[T]): Matcher[Array[T]] = ArrMatcher[T](expected.toSeq)
  def arrMatch[T](expected: Iterator[T]): Matcher[Array[T]] = ArrMatcher[T](expected.toSeq)
}

