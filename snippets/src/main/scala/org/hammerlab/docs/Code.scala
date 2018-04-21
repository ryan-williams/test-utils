package org.hammerlab.docs

import hammerlab.lines._
import org.hammerlab.cmp.CanEq
import org.hammerlab.docs.Code.Example.Render
import org.hammerlab.docs.Code.Setup
import org.hammerlab.docs.Code.Setup.MacroImpl
import org.hammerlab.lines.Lines.unrollIndents

import scala.annotation.{ StaticAnnotation, compileTimeOnly }
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

/**
 * Interface for Scala code-samples that can be inserted into docs as strings but also statically-checked for
 * correctness.
 *
 * This can mean verifying simply that they compile (given the library-code that they exercise), or that sample input-
 * and output-expressions' values match at run-time.
 */
sealed trait Code
object Code {

  def lines(implicit render: Render): ToLines[Code] = {
    /**
     *  Don't import product auto-derivations that would supersede implementations' companions' [[ToLines]] instances
     *
     *  TODO: export these via {{{import hammerlab.lines.generic.coproduct._}}}
     */
    import hammerlab.lines.generic.{ traitToLines, ccons, cnil }
    traitToLines
  }

  implicit def liftComment(comment: String): Code = Comment(comment)

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

    def block[T](expr: T): Setup = macro blockImpl

    def blockImpl(c: Context)(expr: c.Tree): c.Tree = {
      import c.universe._
      val setup =
        expr match {
          case Block(stats, expr) ⇒
            make(c)(stats)
          case t ⇒
            make(c)(List(t))
        }

      q"{..${Seq(expr, setup)}}"
    }

    def make(c: Context)(body: List[c.Tree]): c.Tree = {
      import c.universe._
      import hammerlab.show._
      implicit val showTree: Show[Tree] = {
        (t: Tree) ⇒
          val pos = t.pos
          val content = new String(pos.source.content)
          val start = rewindToLineStart(content, pos.start)
          val lines =
            stripMargin(
              content
                .slice(
                  start,
                  pos.end
                )
                .split("\n")
            )

          lines.mkString("\n")
      }

      val strings =
        body
          .filter {
            s ⇒
              val pos = s.pos
              pos.end > pos.start
          }
          .map { _.show }
          .map { s ⇒ Literal(Constant(s)) }

      val lines = q"_root_.org.hammerlab.lines.Lines(..$strings)"

      q"_root_.org.hammerlab.docs.Code.Setup($lines)"
    }

    object MacroImpl {
      def impl(c: Context)(annottees: c.Expr[Any]*): c.Tree = {
        import c.universe._

        val inputs =
          annottees
            .map(_.tree)
            .toList

        val outputs =
          inputs
            .map {
              case ClassDef(
                mods,
                name,
                tparams,
                Template(
                  parents,
                  self,
                  body
                )
              ) ⇒

                val setup = make(c)(body)

                val term = TermName(name.toString)

                val valdef = q"implicit val $term = $setup"

                val cls =
                  ClassDef(
                    mods,
                    name,
                    tparams,
                    Template(
                      parents,
                      self,
                      valdef :: body
                    )
                  )

                val stmts =
                  Seq(
                    cls,
                    q"object $term extends $name",
                    q"import $term._"
                  )

                q"..$stmts"
              case l ⇒ l
            }

        q"{..$outputs}"
      }
    }
  }

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

  // Capture any indentation preceding the start of an expression, to correctly infer relative indentation of its
  // lines (in the case that there's more than one)
  def rewindToLineStart(content: String, start: Int, requireWhitespace: Boolean = false) = {
    val prevNewline = content.lastIndexOfSlice("\n", start - 1)

    if (
      prevNewline >= 0 &&
        content
          .slice(
            prevNewline + 1,
            start
          )
          .forall(_.isWhitespace || !requireWhitespace)
    )
      prevNewline + 1
    else
      start
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

        val lhs =
          code(
            rewindToLineStart(
              content,
              lstart
            ),
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

@compileTimeOnly("enable macro paradise to expand macro annotations")
class block
  extends StaticAnnotation {
  def macroTransform(annottees: Any*): Unit = macro MacroImpl.impl
}

object block {
  def apply[T](expr: T): Setup = macro Setup.blockImpl
}
