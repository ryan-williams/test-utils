package org.hammerlab.test.matchers.files

import org.hammerlab.test.matchers.files.DirMatcher.dirMatch
import org.hammerlab.test.matchers.utils.MatcherResultTest
import org.hammerlab.test.resources.File

class DirMatcherTest extends MatcherResultTest {

  def check(expectedDir: String, actualDir: String, failureMessage: String = ""): Unit =
    checkResult(dirMatch(expectedDir)(File(actualDir)), failureMessage)

  test("matching dirs") {
    check("a", "c")
  }

  test("differing file") {
    check(
      "a",
      "b",
      """Differing files:
        |
        |	a/numbers:
        |		"1
        |2
        |3
        |[]" was not equal to "1
        |2
        |3
        |[4
        |5
        |6
        |]"
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
        |	a/numbers:
        |		"1
        |2
        |[3
        |]" was not equal to "1
        |2
        |[]"
        |
        |"""
    )
  }
}
