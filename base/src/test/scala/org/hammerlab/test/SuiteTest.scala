package org.hammerlab.test

class SuiteTest
  extends Suite {
  test("fileCopy") {
    ==(
      fileCopy(
        path("a/numbers"),
        tmpPath("numbers")
      )
      .read,
      """1
        |2
        |3
        |"""
      .stripMargin
    )
  }
}
