package org.hammerlab.docs

import hammerlab.lines._
import org.hammerlab.cmp.CanEq
import org.hammerlab.docs.Code.Example.Render
import org.hammerlab.lines.Lines.unrollIndents

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

sealed trait Code
object Code {

  def lines(elems: Code*)(implicit render: Render) = {
    import hammerlab.lines.generic.{traitToLines, ccons, cnil}
    elems.map(_.lines)
  }

  case class Setup(lines: Lines) extends Code
  object Setup {
    implicit val lines: ToLines[Setup] = _.lines
  }

  case class Example(input: Lines, output: Lines) extends Code
  object Example {
    implicit def lines(implicit render: Render): ToLines[Example] =
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

    trait make {
      def example[L, R](l: L, r: R)(implicit cmp: CanEq[L, R]): Example = macro Macro.example[L, R]
    }

    object make extends make

    object Macro {

      val comma = """(?s)\s*,(\s*.*)""".r
      val brace = """\s*\{\s*""".r

      def stripMargin(lines: Seq[String]): Seq[String] = {
        val spaces =
          lines
            .dropWhile(
              _.forall(
                _.isWhitespace
              )
            )
            .map {
              _
                .takeWhile(_.isWhitespace)
                .length
            }
            .min

        lines
          .map {
            _.drop(spaces)
          }
      }

      def example[
          L: c.WeakTypeTag,
          R: c.WeakTypeTag
      ](
          c: Context
      )(
          l: c.Expr[L],
          r: c.Expr[R]
      )(
        cmp: c.Expr[CanEq[L, R]]
      ):
        c.Tree = {
        import c.universe._

        val lpos = l.tree.pos
        val rpos = r.tree.pos

        val content = lpos.source.content

        val between =
          content
            .subSequence(
              lpos.end,
              rpos.start
            )
            .toString

        def code(start: Int, end: Int) = {
          val lines =
            stripMargin(
              content
                .subSequence(start, end)
                .toString
                .split("\n") match {
                  case lines
                    if lines.head.matches("\\s*\\{\\s*") &&
                      lines.last.matches("\\s*\\}\\s*") ⇒
                    lines
                    .slice(
                      1,
                      lines.size - 1
                    )
                  case lines ⇒ lines
                }
            )

          q"_root_.org.hammerlab.lines.Lines(..$lines)"
        }

        val prevNewline = content.lastIndexOfSlice("\n", lpos.start - 1)

        val lstart =
          if (
            prevNewline >= 0 &&
              content
                .subSequence(
                  prevNewline + 1,
                  lpos.start
                )
                .toString
                .forall(_.isWhitespace)
          )
            prevNewline + 1
          else
            lpos.start

        val lhs =
          code(
            lstart,
            lpos.end
          )

        val rstart =
          between match {
            case comma(extra) ⇒
              extra.indexOf('\n') match {
                case n if n == extra.length ⇒
                  rpos.start - extra.length
                case n ⇒
                  rpos.start - extra.length + n + 1
              }

            case l ⇒
              throw new IllegalStateException(
                s"Unrecognized inter-argument span: '$l'"
              )
          }

        val check = q"$cmp.cmp($l, $r)"

        val rhs =
          code(
            rstart,
            rpos.end
          )

        val verify = q"{($check).foreach(e => throw new _root_.java.lang.AssertionError(e.toString))}"

        val example = q"_root_.org.hammerlab.docs.Code.Example($lhs, $rhs)"

        q"{..${Seq(verify, example)}}"
      }
    }
  }
}
