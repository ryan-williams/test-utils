package org.hammerlab.test.matchers.files

import org.hammerlab.test.Suite
import org.hammerlab.test.matchers.files.FileMatcher.fileMatch
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
        |3[]" was not equal to "1
        |2
        |3[
        |4
        |5
        |6]""""
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

  test("match binary files") {
    var bytes = "binary".readBytes

    var tmp = tmpPath()
    var os = tmp.outputStream
    os.write(bytes)
    os.close()

    tmp should fileMatch("binary")

    bytes(0) = 0x1e  // Actual first byte is 0x1f
    tmp = tmpPath()
    os = tmp.outputStream
    os.write(bytes)
    os.close()

    val result = fileMatch("binary").apply(tmp)
    result.matches should be(false)
    result.failureMessage should be("Array(31, -117, 8, 4, 0, 0, 0, 0) was not equal to Array(30, -117, 8, 4, 0, 0, 0, 0)")
  }
}
