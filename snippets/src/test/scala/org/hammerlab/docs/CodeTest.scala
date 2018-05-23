package org.hammerlab.docs

import hammerlab.Suite
import hammerlab.indent.implicits.spaces2
import hammerlab.lines._
import hammerlab.show._
import org.hammerlab.docs.Code.example

class CodeTest
  extends Suite {
  test("lines") {

    /** Verify that [[org.hammerlab.docs.Code.Example]]s are converted to [[Lines]] correctly */
    example(
      "input",
      "// output"
    )
    .showLines should be(
      """input
        |// output""".stripMargin
    )
  }
}
