package org.hammerlab.docs

import hammerlab.lines._
import org.hammerlab.lines.Lines.unrollIndents

sealed trait Code
object Code {

  def lines(elems: Code*) = {
    import hammerlab.lines.generic.{traitToLines, ccons, cnil}
    elems.map(_.lines)
  }

  case class Setup(lines: Lines) extends Code
  object Setup {
    implicit val lines: ToLines[Setup] = _.lines
  }

  case class Example(input: Lines, output: Lines) extends Code
  object Example {
    implicit def lines(implicit render: Render = Render.nextLineComments): ToLines[Example] =
      ToLines { render(_) }

    sealed trait Render {
      def apply(example: Example): Lines
    }
    object Render {
      implicit val nextLineComments: Render =
        new Render {
          def apply(example: Example): Lines =
            Lines(
              example.input,
              unrollIndents(example.output)
                .map(
                  l ⇒
                    (
                      if (l.str.startsWith("// "))
                        l
                      else
                        l.copy(str = "// " + l.str)
                    ): Lines
                )
                .toSeq
            )
        }
    }
  }

}
