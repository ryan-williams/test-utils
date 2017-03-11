package org.hammerlab.test.implicits

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

/**
 * Instantiate this trait to obtain a bunch of handy implicits for using one type ([[T]]) interchangeably with another
 * ([[U]]; in tests).
 *
 * One gotcha is that is more than one [[Conversions]] instance's implicit members are in scope, they will clobber each
 * other since they'll have the same names; import-renaming can be used per-member to get around this.
 */
class Conversions[T, U](implicit val f: T ⇒ U)

object Conversions {

  def apply[T, U](implicit f: T => U): Conversions[T, U] = macro converterImpl[T, U]

  def converterImpl[T: c.WeakTypeTag, U: c.WeakTypeTag](c: whitebox.Context)(f: c.Expr[T ⇒ U]): c.Tree = {
    import c.universe._

    val tType = implicitly[c.WeakTypeTag[T]].tpe
    val uType = implicitly[c.WeakTypeTag[U]].tpe

    val tBasename = tType.toString.split("\\.").last
    val uBasename = uType.toString.split("\\.").last

    val T = tq"$tType"
    val U = tq"$uType"

    def m(name: String) = TermName(s"${name}_${tBasename}_$uBasename")

    q"""import scala.reflect.ClassTag
        new Conversions[$T, $U] {
          implicit def ${m("convertOpt")}(t: Some[$T]): Option[$U] = t.map(f)
          implicit def ${m("convertTuple2Keys")}[V](t: ($T, V)): ($U, V) = (t._1, t._2)
          implicit def ${m("convertMapKeys")}[V](m: Map[$T, V]): Map[$U, V] = m.map(t ⇒ (f(t._1), t._2))
          implicit def ${m("toTupleList")}[V](s: Seq[($T, V)]): List[($U, V)] = s.map(t ⇒ (f(t._1), t._2)).toList
          implicit def ${m("toTupleArray")}[V: ClassTag](s: Array[($T, V)])(implicit ct: ClassTag[$U]): Array[($U, V)] = s.map(t ⇒ (f(t._1), t._2))
          implicit def ${m("toSeq")}(s: Seq[$T]): Seq[$U] = s.map(f)
          implicit def ${m("toVector")}(s: Seq[$T]): Vector[$U] = s.map(f).toVector
          implicit def ${m("toList")}(s: Seq[$T]): List[$U] = s.map(f).toList
          implicit def ${m("toArray")}(s: Seq[$T])(implicit ct: ClassTag[$U]): Array[$U] = s.map(f).toArray
          implicit def ${m("convertArray")}(s: Array[$T])(implicit ct: ClassTag[$U]): Array[$U] = s.map(f)
        }
      """
  }
}
