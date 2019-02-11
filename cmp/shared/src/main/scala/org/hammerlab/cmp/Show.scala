package org.hammerlab.cmp

/**
 * Dummy wrapper for [[cats.Show]] that provides a default instance based on [[AnyRef.toString]]
 *
 * Used for customizing the display of "diff"s / errors from [[CanEq]] comparisons
 */
trait Show[T] {
  def apply(t: T): String
}
trait LowPriorityShow
  extends Serializable {
  /** Fallback [[Show]] for error-types that don't have a custom [[Show]] provided */
  implicit def showAny[T]: Show[T] = Show { t: T ⇒ t.toString }
}
object Show
  extends LowPriorityShow {
  implicit def fromCats[T](implicit s: cats.Show[T]): Show[T] = Show { s.show }
  def apply[T](fn: T ⇒ String): Show[T] =
    new Show[T] {
      def apply(t: T): String = fn(t)
    }
}
