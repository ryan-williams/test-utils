package org.hammerlab

import org.hammerlab.test.matchers.lines.Line
import org.scalatest.matchers.Matcher

package object test {

  type Cmp[T] = CanEq.Cmp[T]
  val Cmp = CanEq.Cmp

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
