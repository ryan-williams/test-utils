package org.hammerlab.test

class SuiteTest
  extends Suite {

  before {
    b = ""
  }

  var b = "before"

  var numAftersRun = 0

  after {
    numAftersRun += 1
  }

  test("before is cleared") {
    b should be("")
    b = "b2"
  }

  test("check num afters") {
    b should be("")
    numAftersRun should be(1)
  }

  test("check num afters again") {
    b should be("")
    numAftersRun should be(2)
  }

  test("fileCopy") {
    fileCopy(
      path("a/numbers"),
      tmpPath("numbers")
    )
    .read should be(
      """1
        |2
        |3
        |"""
      .stripMargin
    )
  }
}
