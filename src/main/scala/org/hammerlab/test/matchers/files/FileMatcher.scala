package org.hammerlab.test.matchers.files

import java.io.UncheckedIOException
import java.nio.charset.MalformedInputException

import org.hammerlab.paths.Path
import org.hammerlab.test.resources.File
import org.scalatest.Matchers
import org.scalatest.matchers.{ MatchResult, Matcher }

class FileMatcher(expected: Path)
  extends Matcher[Path]
    with Matchers {

  /**
   * @param actual Fully-qualified path to the "actual" file whose contents should be compared to the "expected"
   *                   test-resource file.
   */
  override def apply(actual: Path): MatchResult =
    if (expected.isDirectory)
      be(false).apply(expected.isDirectory)
    else if (actual.isDirectory)
      be(false).apply(actual.isDirectory)
    else {
      try {
        val expectedStr = expected.read
        val actualStr = actual.read

        // Hook into scalatest's usual String-comparison logic, which includes a nice diff summary when the strings
        // differ.
        be(actualStr).apply(expectedStr)
      }
      catch {
        // Catch and re-compare binary files
        case _: MalformedInputException | _: UncheckedIOException ⇒
          be(actual.readBytes).apply(expected.readBytes)
      }
    }
}

object FileMatcher {
  def fileMatch(expectedFile: String): FileMatcher = new FileMatcher(File(expectedFile))
  def fileMatch(expectedFile: File): FileMatcher = new FileMatcher(expectedFile)
}
