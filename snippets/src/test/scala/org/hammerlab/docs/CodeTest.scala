package org.hammerlab.docs

import hammerlab.Suite
import hammerlab.indent.implicits.spaces2
import hammerlab.show._
import org.hammerlab.docs.Code.Example

class CodeTest
  extends Suite {
  test("lines") {
    Code.lines.apply(
      Example(
        "Array(1, 2, 1, 3).sorted",
        "Array(1, 1, 2, 3)"
      )
    )
    .showLines should be(
      "Array(1, 2, 1, 3).sorted\n// Array(1, 1, 2, 3)"
    )
  }
}
