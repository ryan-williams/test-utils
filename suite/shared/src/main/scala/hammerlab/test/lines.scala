package hammerlab.test

import org.hammerlab.test.matchers.lines.HasLines

trait lines
  extends Serializable
    with HasLines

object lines extends lines
