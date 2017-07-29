package org.hammerlab.test.matchers.files

import org.hammerlab.test.Suite
import org.hammerlab.test.matchers.files.FileMatcher._
import org.hammerlab.test.resources.{File, stringToTestPath}

class FileMatcherTest extends Suite {
  test("equal files") {
    File("a/numbers").path should fileMatch("c/numbers")
  }

  test("differing files") {
    val result = fileMatch("c/numbers")("b/numbers")
    result.matches should be(false)
    result.failureMessage should be(
      """"1
        |2
        |3
        |[4
        |5
        |6
        |]" was not equal to "1
        |2
        |3
        |[]""""
      .stripMargin
    )
  }

  test("error on directories") {
    val dir = tmpDir()
    var result = new FileMatcher(dir).apply("a/numbers")
    result.matches should be(false)
    result.failureMessage should be(s"'Expected' file $dir should not be a directory")

    result = fileMatch("a/numbers").apply(dir)
    result.matches should be(false)
    result.failureMessage should be(s"'Actual' file $dir should not be a directory")
  }
}
