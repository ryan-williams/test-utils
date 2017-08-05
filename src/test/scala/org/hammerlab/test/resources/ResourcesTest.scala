package org.hammerlab.test.resources

import java.io.FileNotFoundException
import org.hammerlab.test.Suite

class ResourcesTest extends Suite {
  test("non-existent url") {
    intercept[FileNotFoundException] {
      Url("foo")
    }
  }

  test("non-existent file") {
    intercept[FileNotFoundException] {
      File("foo")
    }
  }

  test("read file") {
    File("a/numbers").read should be(
      """1
        |2
        |3
        |"""
      .stripMargin
    )
  }

  test("read file bytes") {
    File("a/numbers").readBytes should be(
      """1
        |2
        |3
        |"""
      .stripMargin
      .getBytes
    )
  }
}
