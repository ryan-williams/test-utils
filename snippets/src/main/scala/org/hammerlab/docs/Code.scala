package org.hammerlab.docs

import hammerlab.lines._
import org.hammerlab.cmp.CanEq
import org.hammerlab.docs.Code.{ rewindToLineStart, stripMargin }

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

  implicit def liftComment(comment: String): Code = Comment(comment)

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

  def example(lines: Lines*): Example = Example(lines)
}

case class Example(lines: Lines) extends Code
object Example {
  implicit val lines: ToLines[Example] = ToLines { _.lines }

  trait dsl {
    def example[L, R](l: L, r: R)(implicit cmp: CanEq[L, R]): Example = macro Macro.example[L, R]
  }

  object dsl extends dsl

  object Macro {

    val betweenRegex = """(?s)([\s)]*),(.*)""".r
    val brace = """\s*\{\s*""".r
    val whitespace = """\s*""".r

    case class Content(value: String) {
      def apply(start: Int, end: Int) = value.slice(start, end)
    }
    object Content {
      def apply(chars: Array[Char]): Content = Content(new String(chars))
    }

    case class Segment(start: Int, end: Int, value: String)
    object Segment {
      def apply(start: Int, end: Int)(
          implicit content: Content
      ): Segment =
        Segment(
          start,
          end,
          content(start, end)
        )

      implicit def toInt(s: Segment) = s.start
      implicit def toStr(s: Segment) = s.value
    }
    case class Exp(prefix: Segment,
                   expr: Segment,
                   suffix: Segment,
                   value: String,
                   lines: List[String]) {
      val start = prefix.start
      val   end = suffix.  end
    }
    object Exp {
      def apply(prefixStart: Int,
                exprStart: Int,
                exprEnd: Int,
                suffixEnd: Int)(
          implicit content: Content
      ): Exp = {
        val value = content(prefixStart, suffixEnd)
        Exp(
          Segment(prefixStart, exprStart),
          Segment(exprStart, exprEnd),
          Segment(exprEnd, suffixEnd),
          value,
          value.split("\n").toList
        )
      }

      implicit val lines = ToLines[Exp] {
        case Exp(prefix, expr, suffix, _, _) ⇒
          Lines(
            s"prefix: '${prefix.value}'",
            s"  expr: '${  expr.value}'",
            s"suffix: '${suffix.value}'"
          )
      }
    }

    case class Exception(msg: String) extends java.lang.Exception(msg)
    def exception(msg: String) = {
      println(s"ERROR: $msg")
      throw Exception(msg)
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

      import hammerlab.show._
      implicit val showPos: Show[Position] =
        Show {
          p ⇒
            s"${p.start}-${p.end} (${p.line}:${p.column})"
        }

      if (lpos.start == lpos.end)
        exception(
          show"Empty lpos: $lpos; this probably means that this macro failed expanding for some other reason, and was re-run with empty expressions for some reason"
        )

      val content = new String(lpos.source.content)
      implicit val _content = Content(content)

      implicit val showTree: Show[Tree] =
        Show {
          t ⇒
            show"${t.pos}:\n${_content(t.pos.start, t.pos.end)}"
        }

      val exampleStart =
        content.lastIndexOf("example(", lpos.start) +
        "example(".length

      val leftStart =
        exampleStart +
          content
            .drop(exampleStart)
            .takeWhile(_.isWhitespace)
            .length

      val (leftExtra, rightPrefix) =
        content.slice(lpos.end, rpos.start) match {
          case betweenRegex(leftExtra, rightPrefix) ⇒
                           (leftExtra, rightPrefix)
          case s ⇒
            exception(
              s"Invalid section between example arguments: $s"
            )
        }

      val comma = lpos.end + leftExtra.length

      val leftEnd = lpos.end + leftExtra.lastIndexOf(')') + 1

      val left =
        Exp(
          exampleStart,
          leftStart,
          leftEnd,
          comma
        )

      val rightStart =
        comma + 1 +
          rightPrefix
            .takeWhile(_.isWhitespace)
            .length

      import hammerlab.iterator._

      val prefix = rightPrefix.iterator
      def count: Int = {
        if (!prefix.hasNext)
          0
        else
          prefix.next match {
            case '(' ⇒ 1 + count
            case '/' ⇒
              prefix
                .nextOption
                .flatMap {
                  case '/' ⇒
                    prefix
                      .find(_ == '\n')
                      .map(_ ⇒ count)
                  case '*' ⇒
                    prefix
                      .sliding2
                      .find(_ == ('*', '/'))
                      .map(_ ⇒ count)
                  case _ ⇒
                    Some(count)
                }
                .getOrElse(0)
            case _ ⇒
              count
          }
      }

      val rightParends = count

      val (rightEnd, exampleEnd) =
        content
          .drop(rpos.end)
          .iterator
          .zipWithIndex
          .collect {
            case (')', idx) ⇒ idx
          }
          .take(rightParends + 1)
          .map(rpos.end + _)
          .toVector
          .takeRight(2) match {
            case Vector(exampleEnd) ⇒
              (
                exampleEnd,
                exampleEnd
              )
            case Vector(rightEnd, exampleEnd) ⇒
              (
                rightEnd + 1,
                exampleEnd
              )
            case parends ⇒
              exception(
                s"Unexpected rhs close-parends situation: ${parends.mkString(",")}"
              )
          }

      val right =
        Exp(
          comma + 1,
          rightStart,
          rightEnd,
          exampleEnd
        )

      val whole = Segment(exampleStart, exampleEnd)

      val lines =
        whole
          .split("\n")
          .toList match {
            case _ :: Nil ⇒
              List(
                s"${left.value}  //${right.value}"
              )
            case lines ⇒
              val marginSize =
                lines
                  .drop(1)
                  .filterNot(
                    _.forall(
                      ' ' == _
                    )
                  )
                  .map(
                    _
                      .takeWhile(
                        ' ' == _
                      )
                      .length
                  )
                  .min

              val marginStr = " " * marginSize

              def strip(line: String) =
                if (line.forall(_.isWhitespace))
                  ""
                else if (!line.startsWith(marginStr))
                  exception(
                    s"Line doesn't start with $marginSize-space margin: $line"
                  )
                else
                  line.drop(marginSize)

              def stripLines(exp: Exp) =
                exp.lines match {
                  case whitespace() :: lines ⇒
                    lines.map { strip }
                  case lines ⇒
                    exception(
                      s"Expected initial whitespace-only line:\n\t${lines.mkString("\n\t")}"
                    )
                }

              val leftLines = stripLines(left)

              val rightLines =
                stripLines(right)
                  .map {
                    line ⇒
                      val trim = line.dropWhile(_.isWhitespace)
                      if (
                        trim.isEmpty ||
                        trim.startsWith("// ")
                      )
                        line
                      else
                        "// " + line
                  }

              leftLines ++
              rightLines
          }

      //val example = q"_root_.org.hammerlab.docs.Example($lhs, $rhs)"
      val example = q"_root_.org.hammerlab.docs.Example(_root_.org.hammerlab.lines.Lines(..$lines))"

      // call `cmp` to compare the two expressions
      val check = q"$cmp.cmp($l, $r)"

      // verify that no error was returned
      val verify = q"{($check).foreach(e => throw new _root_.java.lang.AssertionError(e.toString))}"

      q"{..${Seq(verify, example)}}"
    }
  }
}

case class Comment(lines: String*) extends Code
object Comment {
  implicit def lines: ToLines[Comment] =
    ToLines {
      _
        .lines
        .map {
          case   "" ⇒ ""           : Lines
          case line ⇒ "// " + line : Lines
        }
    }

  trait dsl {
    def comment(comments: String*): Comment = Comment(comments: _*)
  }
}

case class Setup(lines: Lines) extends Code
object Setup {
  implicit val lines: ToLines[Setup] = ToLines { _.lines }

  trait dsl {
    def block[T](expr: T): Setup = macro blockImpl
    def setup[T](expr: T): Setup = macro blockImpl

    @compileTimeOnly("enable macro paradise to expand macro annotations")
    trait Block {
      self: StaticAnnotation ⇒
      def macroTransform(annottees: Any*): Unit = macro MacroImpl.impl
    }
  }
  object dsl extends dsl

  def blockImpl(c: Context)(expr: c.Tree): c.Tree = {
    import c.universe._
    val setup =
      expr match {
        case Block(stats, _) ⇒
          make(c)(stats)
        case t ⇒
          make(c)(List(t))
      }

    q"{..${Seq(expr, setup)}}"
  }

  def make(c: Context)(body: List[c.Tree]): c.Tree = {
    import c.universe._
    import hammerlab.show._
    implicit val showTree: Show[Tree] =
      Show {
        t: Tree ⇒
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

    q"_root_.org.hammerlab.docs.Setup($lines)"
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

              cls
            case l ⇒ l
          }

      q"{..$outputs}"
    }
  }
}

trait dsl
  extends Example.dsl
     with Comment.dsl
     with   Setup.dsl

object dsl extends dsl
