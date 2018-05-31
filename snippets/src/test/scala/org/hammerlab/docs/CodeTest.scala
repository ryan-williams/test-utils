package org.hammerlab.docs

import hammerlab.Suite
import hammerlab.indent.spaces
import hammerlab.lines._
import hammerlab.show._
import org.hammerlab.docs.Code.example

class CodeTest
  extends Suite {
  test("lines") {

    /** Verify that [[org.hammerlab.docs.Example]]s are converted to [[Lines]] correctly */
    val e =
      example(
        "input",
        "// output"
      )

    e.showLines should be(
      """input
        |// output""".stripMargin
    )

    (e: Code).showLines should be(
      """input
        |// output""".stripMargin
    )
  }
}
