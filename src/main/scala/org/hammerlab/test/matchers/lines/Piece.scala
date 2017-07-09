package org.hammerlab.test.matchers.lines

import org.hammerlab.test.matchers.lines.Chars.escape

sealed trait Piece {
  def matches(chars: BufferedIterator[Char]): Boolean
}

object Piece {
  implicit def makeStringPiece(str: String): Piece =
    StringPiece(str)
}

case class StringPiece(str: String)
  extends Piece {

  override def toString: String = str

  override def matches(chars: BufferedIterator[Char]): Boolean = {
    val it =
      str
        .iterator
        .buffered

    while (
      it.hasNext &&
        chars.hasNext &&
        it.head == chars.head
    ) {
      it.next
      chars.next
    }

    it.isEmpty
  }
}

case class Chars(str: String,
                 min: Int = 1,
                 max: Int = Integer.MAX_VALUE)
  extends Piece {

  def check(char: Char) = expectedChars(char)

  override def toString: String =
    str
      .map(escape)
      .mkString("[", "", "]")

  val expectedChars = str.toSet

  override def matches(chars: BufferedIterator[Char]): Boolean = {
    var idx = 0
    while (
      chars.hasNext &&
        check(chars.head) &&
        idx < max
    ) {
      chars.next
      idx += 1
    }

    min <= idx
  }
}

object Chars {
  val escape =
    Map(
      '\n' → "\\n",
      '\t' → "\\t",
      '^' → "\\^",
      '-' → "\\-",
      '[' → "\\[",
      ']' → "\\]"
    )
    .withDefault(identity)
}

class NotChars(override val str: String,
               override val min: Int = 1,
               override val max: Int = Integer.MAX_VALUE)
  extends Chars(str, min, max) {
  override def check(char: Char) =
    !expectedChars(char)
}

object NotChars {
  def apply(str: String,
            min: Int = 1,
            max: Int = Integer.MAX_VALUE): NotChars =
    new NotChars(
      str,
      min,
      max
    )
}

object NotChar {
  def apply(ch: Char,
            min: Int = 1,
            max: Int = Integer.MAX_VALUE): NotChars =
    NotChars(ch.toString)
}

object LineNumber
  extends Chars(
    "0123456789",
    max = 8  // 1e9-1 lines should be enough for any source file 😎
  )

object NewLine extends Chars("\n", max = 1)

