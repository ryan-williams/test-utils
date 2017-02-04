package org.hammerlab.test.implicits

/**
 * Helpers for constructing implicits to get triple-equals checks to work.
 */
object Templates {
  def convertMap[K, V, K1, V1](m: Map[K, V])(implicit fk: K ⇒ K1, fv: V ⇒ V1): Map[K1, V1] =
    m.map(t ⇒ (fk(t._1), fv(t._2)))
}
