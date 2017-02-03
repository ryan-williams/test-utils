package org.hammerlab.test.implicits

import scala.reflect.ClassTag

trait Conversions[T, U] {
  implicit def convertSome(ot: Some[T])(implicit f: T ⇒ U): Option[U] = ot.map(f)
  implicit def convertOpt(ot: Option[T])(implicit f: T ⇒ U): Option[U] = ot.map(f)

  implicit def convertTuple2Keys[V](t: (T, V))(implicit f: T ⇒ U): (U, V) = (t._1, t._2)
  implicit def convertMapKeys[V](m: Map[T, V])(implicit f: T ⇒ U): Map[U, V] = m.map(t ⇒ (f(t._1), t._2))
  implicit def toTupleList[V](s: Seq[(T, V)])(implicit f: T ⇒ U): List[(U, V)] = s.map(t ⇒ (f(t._1), t._2)).toList
  implicit def toTupleArray[V: ClassTag](s: Array[(T, V)])(implicit f: T ⇒ U): Array[(U, V)] = s.map(t ⇒ (f(t._1), t._2))

  implicit def toSeq(s: Seq[T])(implicit f: T ⇒ U): Seq[U] = s.map(f)
  implicit def toVector(s: Seq[T])(implicit f: T ⇒ U): Vector[U] = s.map(f).toVector
  implicit def toList(s: Seq[T])(implicit f: T ⇒ U): List[U] = s.map(f).toList

  implicit def toArray(s: Seq[T])(implicit f: T ⇒ U, ct: ClassTag[U]): Array[U] = s.map(f).toArray
  implicit def convertArray(s: Array[T])(implicit f: T ⇒ U, ct: ClassTag[U]): Array[U] = s.map(f)
}
