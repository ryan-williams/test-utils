package hammerlab

import org.hammerlab.docs.{ Setup, dsl }

import scala.annotation.StaticAnnotation

 trait docs extends dsl
object docs extends docs {
  object dsl extends org.hammerlab.docs.dsl {
    class block extends StaticAnnotation with Setup.dsl.Block
  }
  class block extends StaticAnnotation with Setup.dsl.Block
}
