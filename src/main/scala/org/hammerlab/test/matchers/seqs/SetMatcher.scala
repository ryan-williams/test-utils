package org.hammerlab.test.matchers.seqs

import org.scalatest.matchers.{ MatchResult, Matcher }

case class SetMatcher[T](expected: Set[T])
  extends Matcher[Set[T]] {
  val seqMatcher =
    SeqMatcher[T](
      expected.toSeq,
      matchOrder = false
    )

  override def apply(actual: Set[T]): MatchResult =
    seqMatcher(actual.toSeq)
}

object SetMatcher {
  def setMatch[T](expected: Iterable[(T)]): Matcher[Set[T]] =
    SetMatcher[T](expected.toSet)
}
