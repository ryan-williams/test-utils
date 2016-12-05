package org.hammerlab.test.matchers.seqs

import org.scalatest.matchers.{ MatchResult, Matcher }

class SetMatcher[T](expected: Set[T]) extends Matcher[Set[T]] {
  val seqMatcher = SeqMatcher[T](expected.toSeq, matchOrder = false)
  override def apply(actual: Set[T]): MatchResult = seqMatcher.apply(actual.toSeq)
}

object SetMatcher {
  def setMatch[T](expected: Iterable[(T)]): Matcher[Set[T]] = new SetMatcher[T](expected.toSet)
}
