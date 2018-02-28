package org.hammerlab.cmp.double

import cats.Show
import cats.Show.show
import cats.syntax.show._
import org.hammerlab.math.syntax.E
import org.hammerlab.math.syntax.FuzzyCmp._
import org.hammerlab.test.Cmp

case class Neq(l: Double, r: Double, ε: E)
object Neq {
  def apply(l: Double, r: Double)(implicit ε: E): Option[Neq] =
    if (l === r)
      None
    else
      Some(
        Neq(l, r, ε)
      )

  implicit def showDoubleNeq(implicit sd: Show[Double]): Show[Neq] =
    show {
      case Neq(x, y, ε) ⇒
        show"(x, y): ($x, $y); x/y ${x/y} y/x ${y/x}, ε: $ε"
    }
}

trait HasNeq {
  /**
   * Consider [[Double]]s to be equal if their ratio if their ratio differs from [[1]] by less than or equal to [[ε]],
   * or if one of them equals zero and the other's absolute value is less than or equal to [[ε]]
   */
  implicit def doubleCmp(implicit ε: E): Cmp.Aux[Double, Neq] =
    Cmp[Double, Neq](Neq(_, _))
}
