package org.hammerlab.test.matchers.seqs

import org.scalatest.matchers.{ MatchResult, Matcher }

case class MapMatcher[K: Ordering, V: Ordering](expected: Map[K, V]) extends Matcher[Map[K, V]] {
  val seqMatcher = PairSeqMatcher[K, V](expected.toSeq, matchOrder = false)

  override def apply(actual: Map[K, V]): MatchResult = {
    seqMatcher.apply(actual.toSeq)
  }
}

object MapMatcher {
  def mapMatch[K: Ordering, V: Ordering](expected: Map[K, V]): Matcher[Map[K, V]] = MapMatcher(expected)
}
