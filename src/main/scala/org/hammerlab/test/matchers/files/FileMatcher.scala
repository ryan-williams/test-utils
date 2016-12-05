package org.hammerlab.test.matchers.files

import java.nio.charset.MalformedInputException
import java.nio.file.{ Files, Paths }

import org.hammerlab.test.resources.File
import org.scalatest.Matchers
import org.scalatest.matchers.{ MatchResult, Matcher }

import scala.io.Source.fromFile

class FileMatcher(expectedFile: String) extends Matcher[String] with Matchers {
  /**
   * @param actualFile Fully-qualified path to the "actual" file whose contents should be compared to the "expected"
   *                   test-resource file.
   */
  override def apply(actualFile: String): MatchResult = {
    val expectedPath = File(expectedFile)
    try {
      val expectedStr = expectedPath.read
      val actualStr = fromFile(actualFile).mkString

      // Hook into scalatest's usual String-comparison logic, which includes a nice diff summary when the strings
      // differ.
      be(actualStr).apply(expectedStr)
    } catch {
      case e: MalformedInputException ⇒
        be(Files.readAllBytes(Paths.get(actualFile))).apply(expectedPath.readBytes)
    }
  }
}

object FileMatcher {
  def fileMatch(actualFile: String): FileMatcher = new FileMatcher(actualFile)
}
