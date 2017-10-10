package org.hammerlab.test.matchers.lines

case class Line(pieces: Seq[Piece]) {
  def ++(piece: Piece): Line =
    copy(
      pieces =
        pieces :+
          piece
    )

  def ::(line: Line): Line =
    copy(
      pieces =
        pieces ++ line.pieces
    )
}

object Line {
  implicit def lineFromPiece(piece: Piece): Line = Line(Vector(piece))
  implicit def lineFromString(str: String): Line = Line(Vector[Piece](str))

  implicit class LineContextHelper(val sc: StringContext) extends AnyVal {
    def l(args: Piece*): Line = {

      def part(str: String): Option[Piece] =
        if (str.nonEmpty)
          Some(StringPiece(str))
        else
          None

      val parts = sc.parts.iterator.map(part)
      val pieces = args.iterator

      val first = parts.next
      val rest =
        parts
          .zip(pieces)
          .flatMap {
            case (part, arg) ⇒
              Seq(
                Some(arg),
                part
              )
          }
          .flatten
          .toList

      Line(
        first
          .map {
            _ :: rest
          }
          .getOrElse(
            rest
          )
      )
    }
  }

  val d = Digits
  val ln = LineNumber
}
