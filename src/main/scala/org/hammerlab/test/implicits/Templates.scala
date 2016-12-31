package org.hammerlab.test.implicits

import scala.reflect.ClassTag

/**
 * Helpers for constructing implicits to get triple-equals checks to work.
 */
trait Templates {
  def convertTuple2[A, B, A1, B1](t: (A, B))
                                 (implicit f1: A => A1, f2: B => B1): (A1, B1) =
    (f1(t._1), f2(t._2))

  def convertKey[A, B, A1](t: (A, B))(implicit f: A ⇒ A1): (A1, B) = (t._1, t._2)

  def convertTuple3[A, B, C, A1, B1, C1](t: (A, B, C))
                                        (implicit f1: A => A1, f2: B => B1, f3: C => C1): (A1, B1, C1) =
    (f1(t._1), f2(t._2), f3(t._3))

  def convertTuple4[A, B, C, D, A1, B1, C1, D1](t: (A, B, C, D))
                                               (implicit f1: A => A1, f2: B => B1, f3: C => C1, f4: D ⇒ D1): (A1, B1, C1, D1) =
    (f1(t._1), f2(t._2), f3(t._3), f4(t._4))

  def toSeq[T, U](s: Seq[T])(implicit f: T ⇒ U): Seq[U] = s.map(f)
  def arrToSeq[T, U](s: Array[T])(implicit f: T ⇒ U): Seq[U] = s.map(f)

  def toVector[T, U](s: Seq[T])(implicit f: T ⇒ U): Vector[U] = s.map(f).toVector
  def arrToVector[T, U](s: Array[T])(implicit f: T ⇒ U): Vector[U] = s.map(f).toVector

  def toList[T, U](s: Seq[T])(implicit f: T ⇒ U): List[U] = s.map(f).toList
  def arrToList[T, U](s: Array[T])(implicit f: T ⇒ U): List[U] = s.map(f).toList

  def toArray[T, U: ClassTag](s: Seq[T])(implicit f: T ⇒ U): Array[U] = s.map(f).toArray
  def convertArray[T, U: ClassTag](s: Array[T])(implicit f: T ⇒ U): Array[U] = s.map(f)

  def convertMap[K, V, K1, V1](m: Map[K, V])(implicit fk: K ⇒ K1, fv: V ⇒ V1): Map[K1, V1] =
    m.map(t ⇒ (fk(t._1), fv(t._2)))

  implicit def rangeToArray(range: Range): Array[Int] = range.toArray

  def convertOpt[T, U](o: Option[T])(implicit f: T ⇒ U): Option[U] = o.map(f)
}
