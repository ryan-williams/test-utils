package org.hammerlab.docs

import hammerlab.lines._
import org.hammerlab.cmp.CanEq
import org.hammerlab.docs.Code.Example.Render
import org.hammerlab.docs.Code.Setup.MacroImpl
import org.hammerlab.lines.Lines.unrollIndents

import scala.annotation.{ StaticAnnotation, compileTimeOnly }
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

sealed trait Code
object Code {

  def lines(implicit render: Render): ToLines[Code] = {
    import hammerlab.lines.generic.{ traitToLines, ccons, cnil }
    traitToLines
  }

  case class Comment(lines: String*) extends Code
  object Comment {
    implicit def lines: ToLines[Comment] = {
      case Comment(lines @ _*) ⇒
        Lines(
          lines
            .map {
              case   "" ⇒ ""           : Lines
              case line ⇒ "// " + line : Lines
            }
        )
    }
  }

  case class Setup(lines: Lines) extends Code
  object Setup {
    implicit val lines: ToLines[Setup] = _.lines

    object MacroImpl {
      def impl(c: Context)(annottees: c.Expr[Any]*): c.Tree = {
        import c.universe._

        import hammerlab.show._
        implicit val showTree: Show[Tree] = {
          (t: Tree) ⇒
            val pos = t.pos
            pos.source.content.subSequence(pos.start, pos.end).toString
        }

        val inputs =
          annottees
            .map(_.tree)
            .toList

        val outputs =
          inputs
            .map {
              case (c: ClassDef) ⇒
                val pos = c.pos
                val src = pos.source.content
                val impl = c.impl
                val body = impl.body

                val strings =
                  body
                    .map    { _.show     }
                    .filter { _.nonEmpty }
                    .map    { s ⇒ Literal(Constant(s)) }

                val lines = q"_root_.org.hammerlab.lines.Lines(..$strings)"

                val setup = q"_root_.org.hammerlab.docs.Code.Setup($lines)"

                val valdef = q"implicit val ${TermName(c.name.toString)} = $setup"

                val name = c.name

                val cls =
                  ClassDef(
                    c.mods,
                    name,
                    c.tparams,
                    Template(
                      impl.parents,
                      impl.self,
                      valdef :: body
                    )
                  )

                val term = TermName(name.toString)

                val obj = q"object $term extends $name"

                val stmts = Seq(cls, obj, q"import $term._")
                q"..$stmts"
              case l ⇒ l
            }

        q"{..$outputs}"
      }
    }
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
                .map {
                  case l @ Line(s, _) ⇒
                    (
                      if (s.isEmpty || s.startsWith("// "))
                        l
                      else
                        l.prepend("// ")
                    ): Lines
                }
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
        val margins =
          lines
            .filterNot(
              _.forall(
                _.isWhitespace
              )
            )
            .map {
              _
                .takeWhile(_.isWhitespace)
                .length
            }

        val spaces =
          if (margins.isEmpty)
            0
          else
            margins.min

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

        val content = new String(lpos.source.content)

        val lstart =
          content(lpos.start - 1) match {
            case '('
              if lpos.start >= "example(".length &&
                 content.substring(
                   lpos.start - "example(".length,
                   lpos.start
                 ) != "example(" ⇒
              lpos.start - 1
            case _ ⇒
              lpos.start
          }

        val lend =
          lpos.end match {
            case end
              if end < content.length &&
                 content(end) == ')' ⇒
              end + 1
            case end ⇒
              end
          }

        val between =
          content
            .slice(
              lend,
              rpos.start
            )

        def code(start: Int, end: Int) = {
          val lines =
            stripMargin(
              content
                .slice(start, end)
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

        val prevNewline = content.lastIndexOfSlice("\n", lstart - 1)

        // capture any indentation preceding the start of the LHS expression, to correctly infer relative indentation of
        // its lines
        val llinestart =
          if (
            prevNewline >= 0 &&
            content
              .slice(
                prevNewline + 1,
                lstart
              )
              .forall(_.isWhitespace)
          )
            prevNewline + 1
          else
            lstart

        val lhs =
          code(
            llinestart,
            lend
          )

        // drop whitespace between [the comma separating the LHS- and RHS-expressions] and [the start of the RHS, or the
        // line on which it starts]
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

        // call `cmp` to compare the two expressions
        val check = q"$cmp.cmp($l, $r)"

        // verify that no error was returned
        val verify = q"{($check).foreach(e => throw new _root_.java.lang.AssertionError(e.toString))}"

        val rhs =
          code(
            rstart,
            rpos.end
          )

        val example = q"_root_.org.hammerlab.docs.Code.Example($lhs, $rhs)"

        q"{..${Seq(verify, example)}}"
      }
    }
  }
}

@compileTimeOnly("enable macro paradise to expand macro annotations")
class setup
  extends StaticAnnotation {
  def macroTransform(annottees: Any*): Unit = macro MacroImpl.impl
}
