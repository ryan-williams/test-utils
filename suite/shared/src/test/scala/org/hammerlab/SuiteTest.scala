package org.hammerlab

import org.hammerlab.cmp.Cmp

class SuiteTest
  extends Suite {

  before {
    b = ""
  }

  var b: String = "before"

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
