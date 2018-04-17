package org.hammerlab.docs

import org.hammerlab.cmp.CanEq
import org.hammerlab.docs.Code.Example

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

object Macros {
  def example[L, R](l: L, r: R)(implicit cmp: CanEq[L, R]): Example = macro exampleImpl[L, R]

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

//    println(s"found $spaces-space margin:\n${lines.mkString("\n")}\n")

    lines
      .map {
        _.drop(spaces)
      }
  }

  def exampleImpl[
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

    def code(start: Int, end: Int) =
      Literal(
        Constant(
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
          .mkString("\n")
        )
      )

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

    //println(s"lhs: ${lpos.start}:${lpos.end} - ${lpos.line}:${lpos.column}, nl $prevNewline:\n${content.subSequence(lpos.start, lpos.end).toString}")

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
