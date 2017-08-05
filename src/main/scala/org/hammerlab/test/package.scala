package org.hammerlab

import org.hammerlab.test.matchers.lines.Line
import org.scalatest.matchers.Matcher

package object test {
  def firstLinesMatch(lines: Line*): Matcher[String] =
    org.hammerlab.test.matchers.lines.Matcher(
      prefix = true,
      lines: _*
    )

  def linesMatch(lines: Line*): Matcher[String] =
    org.hammerlab.test.matchers.lines.Matcher(
      prefix = false,
      lines: _*
    )
}
