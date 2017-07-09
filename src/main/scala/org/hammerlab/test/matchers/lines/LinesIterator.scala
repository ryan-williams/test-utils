package org.hammerlab.test.matchers.lines

case class Pos(line: Int, idx: Int) {
  override def toString: String = s"$line:$idx"
}

case class LinesIterator(str: String)
  extends BufferedIterator[Char] {

  val it = str.iterator.buffered
  var pos = Pos(0, 0)

  override def head: Char = it.head
  override def hasNext: Boolean = it.hasNext

  override def next(): Char = {
    val n = it.next

    if (n == '\n')
      pos = pos.copy(line = pos.line + 1, idx = 0)
    else
      pos = pos.copy(idx = pos.idx + 1)

    n
  }
}
