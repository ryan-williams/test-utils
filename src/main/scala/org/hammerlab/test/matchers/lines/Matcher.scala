package org.hammerlab.test.matchers.lines

import org.hammerlab.test.matchers.lines.Chars.escape
import org.scalatest.matchers
import org.scalatest.matchers.MatchResult

object Matcher {
  def apply(lines: Line*): matchers.Matcher[String] =
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
          def strSample =
            chars
              .map(escape)
              .take(100)
              .mkString("")

          def piecesSample =
            pieces
              .take(100)
              .mkString(", ")

          if (!pieces.hasNext)
            s"Actual string extends beyond expected ($strPos): $strSample"
          else if (!chars.hasNext)
            s"Actual string ends before expected ($strPos); missing: $piecesSample"
          else
            s"${pieces.head} didn't match string from $strPos: $strSample"
        }

        MatchResult(
          pieces.isEmpty && chars.isEmpty,
          unexpectedMismatchMessage,
          s"Unexpected match:\n$str"
        )
      }
    }
}
