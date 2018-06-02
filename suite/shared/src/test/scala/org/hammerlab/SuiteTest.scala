package org.hammerlab

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
    ==(b, "")
    b = "b2"
  }

  test("check num afters") {
    ==(b, "")
    ==(numAftersRun, 1)
  }

  test("check num afters again") {
    ==(b, "")
    ==(numAftersRun, 2)
  }
}
