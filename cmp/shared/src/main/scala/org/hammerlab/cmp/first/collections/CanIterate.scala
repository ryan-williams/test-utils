package org.hammerlab.cmp.first.collections

trait CanIterate[T[_]] {
  def apply[A](t: T[A]): Iterator[A]
}

object CanIterate {
  implicit val array: CanIterate[Array] =
    new CanIterate[Array] {
      def apply[A](t: Array[A]) = t.toIterator
    }
  implicit def option[Opt[T] <: Option[T]]: CanIterate[Opt] =
    new CanIterate[Opt] {
      def apply[A](t: Opt[A]) = t.toIterator
    }
  implicit val iterator: CanIterate[Iterator] =
    new CanIterate[Iterator] {
      def apply[A](t: Iterator[A]) = t
    }
  implicit def traversable[I[T] <: scala.collection.Traversable[T]]: CanIterate[I] =
    new CanIterate[I] {
      def apply[A](t: I[A]) = t.toIterator
    }
}
