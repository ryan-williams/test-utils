package org.hammerlab.test.matchers.seqs

import org.scalatest.matchers.{ MatchResult, Matcher }

import scala.collection.mutable.ArrayBuffer

case class SeqMatcher[T](expected: Seq[T], matchOrder: Boolean = true) extends Matcher[Seq[T]] {
  override def apply(actual: Seq[T]): MatchResult = {
    val expectedSet: Set[T] = expected.toSet

    val actualSet: Set[T] = actual.toSet

    val errors = ArrayBuffer[String]()
    errors += "Sequences didn't match!"
    errors += ""

    val extraElems = actualSet.diff(expectedSet)
    val missingElems = expectedSet.diff(actualSet)

    if (extraElems.nonEmpty || missingElems.nonEmpty) {

      if (extraElems.nonEmpty) {
        errors += s"Extra elems:"
        errors += extraElems.mkString("\t", "\n\t", "\n")
      }

      if (missingElems.nonEmpty) {
        errors += s"Missing elems:"
        errors += missingElems.mkString("\t", "\n\t", "\n")
      }
    } else if (matchOrder && actual != expected) {
      errors += s"Elements out of order:"
      errors += "Expected:"
      errors += expected.mkString("\t", "\n\t", "\n")
      errors += "Actual:"
      errors += actual.mkString("\t", "\n\t", "\n")
    }

    val matched =
      if (matchOrder)
        actual == expected
      else
        actualSet == expectedSet

    MatchResult(
      matched,
      errors.mkString("\n"),
      s"$actual matched; was supposed to not."
    )
  }
}

object SeqMatcher {
  def seqMatch[T](expected: Iterable[T]): Matcher[Seq[T]] = SeqMatcher[T](expected.toSeq)
  def seqMatch[T](expected: Array[T]): Matcher[Seq[T]] = SeqMatcher[T](expected.toList)
  def seqMatch[T](expected: Iterator[T]): Matcher[Seq[T]] = SeqMatcher[T](expected.toSeq)
}
