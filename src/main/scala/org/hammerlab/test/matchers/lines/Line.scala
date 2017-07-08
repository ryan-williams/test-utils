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
}
