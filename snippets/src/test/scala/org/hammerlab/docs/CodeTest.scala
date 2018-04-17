package org.hammerlab.docs

import hammerlab.Suite
import hammerlab.indent.implicits.spaces2
import hammerlab.show._
import org.hammerlab.docs.Code.Example
import Macros.example

class CodeTest
  extends Suite {
  test("lines") {
    Code.lines(
      Example(
        "Array(1, 2, 1, 3).sorted",
        "Array(1, 1, 2, 3)"
      )
    )
    .map(_.show) should be(
      Seq(
        "Array(1, 2, 1, 3).sorted\n// Array(1, 1, 2, 3)"
      )
    )

//    Code.make(
//      example(
//        Array(1, 2, 1, 3).sorted,
//        Array(1, 1, 2, 3)
//      )
//    )
  }
}
