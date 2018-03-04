package org.hammerlab.test.matchers.seqs

import org.scalatest.matchers.{ MatchResult, Matcher }

// Operate on common superclass of collection.{im,}mutable.Map
import collection.Map

case class MapMatcher[K: Ordering, V: Ordering](expected: Map[K, V]) extends Matcher[Map[K, V]] {
  val seqMatcher = PairSeqMatcher[K, V](expected.toSeq, matchOrder = false)

  override def apply(actual: Map[K, V]): MatchResult = {
    seqMatcher.apply(actual.toSeq)
  }
}

trait HasMapMatcher {
  def mapMatch[K: Ordering, V: Ordering](expected: Map[K, V]): Matcher[Map[K, V]] = MapMatcher(expected)
  def mapMatch[K: Ordering, V: Ordering](expected: (K, V)*): Matcher[Map[K, V]] = MapMatcher(expected.toMap)
}
