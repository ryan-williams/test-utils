package org.hammerlab.test.matchers.files

import org.hammerlab.test.matchers.files.DirMatcher.dirMatch
import org.hammerlab.test.matchers.utils.MatcherResultTest
import org.hammerlab.test.resources.File

class DirMatcherTest extends MatcherResultTest {

  def check(expectedDir: File, actualDir: File, failureMessage: String = ""): Unit =
    checkResult(dirMatch(expectedDir)(actualDir), failureMessage)

  test("matching dirs") {
    check("a", "c")
  }

  test("differing file") {
    check(
      "a",
      "b",
      """Differing files:
        |
        |	numbers:
        |		"1
        |2
        |3
        |[4
        |5
        |6
        |]" was not equal to "1
        |2
        |3
        |[]"
        |
        |"""
    )
  }

  test("missing file") {
    check(
      "a",
      "d",
      """Missing files:
        |
        |	words
        |
        |"""
    )
  }

  test("extra file") {
    check(
      "a",
      "e",
      """Extra files:
        |
        |	more-words
        |
        |"""
    )
  }

  test("all") {
    check(
      "a",
      "f",
      """Extra files:
        |
        |	more-words
        |
        |Missing files:
        |
        |	words
        |
        |Differing files:
        |
        |	numbers:
        |		"1
        |2
        |[]" was not equal to "1
        |2
        |[3
        |]"
        |
        |"""
    )
  }
}
