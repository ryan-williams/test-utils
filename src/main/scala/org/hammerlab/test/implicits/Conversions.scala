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
class Conversions

object Conversions {

  def apply[T, U]: Conversions = macro converterImpl[T, U]

  def converterImpl[T: c.WeakTypeTag, U: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    import c.universe._

    val tType = implicitly[c.WeakTypeTag[T]].tpe
    val uType = implicitly[c.WeakTypeTag[U]].tpe

    val tBasename = tType.toString.split("\\.").last
    val uBasename = uType.toString.split("\\.").last

    val T = tq"$tType"
    val U = tq"$uType"

    def m(name: String) = TermName(s"${name}_${tBasename}_$uBasename")

    q"""import scala.reflect.ClassTag
        new Conversions {
          implicit def ${m("convertOpt")}(t: Some[$T])(implicit f: $T => $U): Option[$U] = t.map(f)
          implicit def ${m("convertTuple2Keys")}[V](t: ($T, V))(implicit f: $T ⇒ $U): ($U, V) = (t._1, t._2)
          implicit def ${m("convertMapKeys")}[V](m: Map[$T, V])(implicit f: $T ⇒ $U): Map[$U, V] = m.map(t ⇒ (f(t._1), t._2))
          implicit def ${m("toTupleList")}[V](s: Seq[($T, V)])(implicit f: $T ⇒ $U): List[($U, V)] = s.map(t ⇒ (f(t._1), t._2)).toList
          implicit def ${m("toTupleArray")}[V: ClassTag](s: Array[($T, V)])(implicit f: $T ⇒ $U, ct: ClassTag[$U]): Array[($U, V)] = s.map(t ⇒ (f(t._1), t._2))
          implicit def ${m("toSeq")}(s: Seq[$T])(implicit f: $T ⇒ $U): Seq[$U] = s.map(f)
          implicit def ${m("toVector")}(s: Seq[$T])(implicit f: $T ⇒ $U): Vector[$U] = s.map(f).toVector
          implicit def ${m("toList")}(s: Seq[$T])(implicit f: $T ⇒ $U): List[$U] = s.map(f).toList
          implicit def ${m("toArray")}(s: Seq[$T])(implicit f: $T ⇒ $U, ct: ClassTag[$U]): Array[$U] = s.map(f).toArray
          implicit def ${m("convertArray")}(s: Array[$T])(implicit f: $T ⇒ $U, ct: ClassTag[$U]): Array[$U] = s.map(f)
        }
      """
  }
}
