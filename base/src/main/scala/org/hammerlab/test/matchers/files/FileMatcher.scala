package org.hammerlab.test.matchers.files

import org.hammerlab.paths.Path
import org.hammerlab.test.resources.{ File, Url }
import org.scalatest.Matchers
import org.scalatest.matchers.{ MatchResult, Matcher }

class FileMatcher(expected: Path, binaryMatch: Boolean = false)
  extends Matcher[Path]
    with Matchers {

  /**
   * @param actual Fully-qualified path to the "actual" file whose contents should be compared to the "expected"
   *               test-resource file.
   */
  override def apply(actual: Path): MatchResult =
    if (expected.isDirectory)
      MatchResult(
        false,
        s"'Expected' file $expected should not be a directory",
        s"<unused>"
      )
    else if (actual.isDirectory)
      MatchResult(
        false,
        s"'Actual' file $actual should not be a directory",
        s"<unused>"
      )
    else {
      val expectedStr = expected.read
      val actualStr = actual.read

      // Hook into scalatest's usual String-comparison logic, which includes a nice diff summary when the strings
      // differ.
      be(expectedStr).apply(actualStr)
    }
}

trait HasFileMatchers {
  def fileMatch(expected: String): FileMatcher = new FileMatcher(Path(Url(expected).toURI))
  def fileMatch(expected: File): FileMatcher = new FileMatcher(expected)
  def fileMatch(expected: Path): FileMatcher = new FileMatcher(expected)
}

object FileMatcher
  extends HasFileMatchers
    with Serializable
