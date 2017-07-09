package org.hammerlab.test.matchers.lines

import org.scalatest.matchers
import org.scalatest.matchers.MatchResult

object Matcher {
  def apply(lines: Line*): matchers.Matcher[String] =
    apply(
      prefix = false,
      lines: _*
    )

  def apply(prefix: Boolean, lines: Line*): matchers.Matcher[String] =
    new matchers.Matcher[String] {
      override def apply(str: String): MatchResult = {

        val chars = LinesIterator(str)

        val pieces =
          lines
            .flatMap(_.pieces :+ NewLine)
            .dropRight(1)
            .iterator
            .buffered

        while (
          pieces.hasNext &&
            pieces
              .head
              .matches(chars)
        ) {
          pieces.next
        }

        val strPos = chars.pos

        def unexpectedMismatchMessage: String = {
          def piecesSample =
            pieces
              .take(100)
              .mkString(", ")

          if (!pieces.hasNext)
            s"Actual string extends beyond expected ($strPos):\n$str"
          else if (!chars.hasNext)
            s"Actual string ends before expected ($strPos); missing: $piecesSample"
          else
            s"${pieces.head} didn't match string from $strPos:\n$str"
        }

        MatchResult(
          pieces.isEmpty && (prefix || chars.isEmpty),
          unexpectedMismatchMessage,
          s"Unexpected match:\n$str"
        )
      }
    }
}
