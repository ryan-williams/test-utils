package hammerlab.test

import org.hammerlab.test.matchers.lines.HasLine

trait lines
  extends Serializable
    with HasLine

object lines extends lines
