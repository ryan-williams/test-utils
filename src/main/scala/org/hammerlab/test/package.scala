package org.hammerlab

import org.hammerlab.test.matchers.lines.Line
import org.scalatest.matchers.Matcher

package object test {
  def linesMatch(lines: Line*): Matcher[String] =
    org.hammerlab.test.matchers.lines.Matcher(lines: _*)
}
