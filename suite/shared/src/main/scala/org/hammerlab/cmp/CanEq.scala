package org.hammerlab.cmp

import cats.Eq
import org.hammerlab.cmp.CanEq.Cmp
import org.hammerlab.cmp.first.collections.Unordered
import shapeless._

import scala.util.matching.Regex

/**
 * Type-class for comparing instances of two (possibly different )types, with a customizable "diff" output-type
 * containing a configurable representation of the "diff", if any
 */
trait CanEq[L, R] {
  type Diff
  def   cmp(l: L, r: R): Option[Diff]
  def apply(l: L, r: R): Option[Diff] = cmp(l, r)
  def   eqv(l: L, r: R):      Boolean = cmp(l, r).isEmpty
}

trait Priority4CanEq
  extends Serializable {

  /**
   * Derive a [[CanEq]] for two types given a [[Cmp]] for teh second and an implicit conversion from the first to the
   * second
   */
  implicit def convertActualCanEq[
    L,
    R
  ](
    implicit
    cmp: Lazy[Cmp[R]],
    evl: L =:!= R,
    fn: L ⇒ R
  ):
    CanEq.Aux[
      L,
      R,
      cmp.value.Diff
    ] =
    CanEq {
      (l, r) ⇒
        cmp.value(l, r)
    }

  /**
   * Derive a [[Cmp]] from an [[Ordering]]; lower priority than [[CanEq.fromEq]] (derivation from [[cats.Eq]])
   */
  implicit def cmpFromOrdering[T](implicit ord: Ordering[T]): Cmp.Aux[T, (T, T)] =
    Cmp {
      (l, r) ⇒
        if (ord.equiv(l, r))
          None
        else
          Some((l, r))
    }
}

trait Priority3CanEq
  extends Priority4CanEq {
  /**
   * Derive a [[CanEq]] for two types given a [[Cmp]] for the first and an implicit conversion from the second to the
   * first
   */
  implicit def convertExpectedCanEq[
    L,
    R
  ](
    implicit
    cmp: Lazy[Cmp[L]],
    evl: L =:!= R,
    fn: R ⇒ L
  ):
    CanEq.Aux[
      L,
      R,
      cmp.value.Diff
    ] =
    CanEq {
      (l, r) ⇒
        cmp.value(l, r)
    }
}

trait Priority2CanEq
  extends Priority3CanEq {

  /**
   * Derive a [[CanEq]] for two types where the second is a ([[=:!= strict!]]) subclass of the first
   */
  implicit def subtypeCanEq[
    L,
    R <: L
  ](
    implicit
    cmp: Lazy[Cmp[L]],
    ev: L =:!= R
  ):
    CanEq.Aux[
      L,
      R,
      cmp.value.Diff
    ] =
    CanEq {
      (l, r) ⇒
        cmp.value(l, r)
    }

  /**
   * Derive a [[CanEq]] for two types where the first is a ([[=:!= strict!]]) subclass of the second
   */
  implicit def supertypeCanEq[
    L <: R,
    R
  ](
    implicit
    cmp: Lazy[Cmp[R]],
    ev: L =:!= R
  ):
    CanEq.Aux[
      L,
      R,
      cmp.value.Diff
    ] =
    CanEq {
      (l, r) ⇒
        cmp.value(l, r)
    }
}

trait Priority1CanEq
  extends Priority2CanEq {

  /**
   * Derive a [[Cmp]] from an [[Eq]], where the returned "diff"-representation is just a [[Tuple2]] with the two
   * non-equal values
   */
  implicit def fromEq[T](implicit e: Eq[T]): Cmp.Aux[T, (T, T)] =
    CanEq {
      (t1, t2) ⇒
        if (e.eqv(t1, t2))
          None
        else
          Some(
            (t1, t2)
          )
    }
}

object Cmp {
  type Aux[T, E] = CanEq.Aux[T, T, E]

  /** Create a [[Cmp]] from its single method */
  def apply[T, E](fn: (T, T) ⇒ Option[E]): Aux[T, E] = CanEq(fn)

  /** Create a [[Cmp]] interms of another */
  def by[L, R](fn: R ⇒ L)(implicit cmp: Cmp[L]): Aux[R, cmp.Diff] =
    Cmp {
      (l, r) ⇒
        cmp(
          fn(l),
          fn(r)
        )
    }

  /**
   * Wrapper around a [[Cmp]] used to enable overloading methods that would otherwise have the same type after erasure
   *
   * See [[CanEq.dsl]] or [[dsl]]
   */
  case class Wrapper[T, D](cmp: Cmp.Aux[T, D]) {
    type Diff = cmp.Diff
  }
  object Wrapper {
    implicit def   wrap[T, D](implicit c: Cmp.Aux[T, D]): Wrapper[T, D] = Wrapper(c)
    implicit def unwrap[T, D](         w: Wrapper[T, D]): Cmp.Aux[T, D] = w.cmp
  }
}

trait Regexs
  extends first.collections.all {
  implicit val stringCanEqRegex: CanEq.Aux[String, Regex, (String, Regex)] =
    new CanEq[String, Regex] {
      override type Diff = (String, Regex)
      override def cmp(l: String, r: Regex): Option[Diff] =
        r
          .findFirstMatchIn(l)
          .fold {
            Option(l, r)
          } {
            _ ⇒ None
          }
    }

  case class StartsWith(str: String)
  implicit val stringCanEqStartsWith: CanEq.Aux[String, StartsWith, (String, String)] =
    new CanEq[String, StartsWith] {
      override type Diff = (String, String)
      override def cmp(l: String, r: StartsWith): Option[Diff] =
        if (l.startsWith(r.str))
          None
        else
          Some((l, r.str))
    }
}

object CanEq
  extends Regexs {

  /** Short-hand for a [[CanEq]] whose comparee-types are equal */
  type Cmp[T] = CanEq[T, T]

  type Aux[L, R, D] = CanEq[L, R] { type Diff = D }

  /**
   * Short-hand for creating [[CanEq]] instances from a single method.
   *
   * TODO: can probably go away in favor of SAM-syntax once Scala 2.11 support is dropped.
   */
  def apply[L, R, D](fn: (L, R) ⇒ Option[D]): Aux[L, R, D] =
    new CanEq[L, R] {
      type Diff = D
      override def cmp(l: L, r: R): Option[Diff] =
        fn(l, r)
    }

  /**
   * Wrapper around a [[CanEq]] used to enable overloading methods that would otherwise have the same type after erasure
   *
   * See [[dsl]] or [[org.hammerlab.cmp.dsl]]
   */
  case class Wrapper[L, R, D](cmp: CanEq.Aux[L, R, D]) {
    type Diff = cmp.Diff
  }
  object Wrapper {
    implicit def   wrap[L, R, D](implicit c: CanEq.Aux[L, R, D]): Wrapper[L, R, D] = Wrapper(c)
    implicit def unwrap[L, R, D](         w: Wrapper[L, R, D]): CanEq.Aux[L, R, D] = w.cmp
  }

  implicit val nothingCanEqNothing: Cmp.Aux[Nothing, Nothing] = Cmp[Nothing, Nothing] { (_, _) ⇒ ??? }

  def by[L, R](
    fn: L ⇒ R
  )(
    implicit
    cmp: Cmp[R]
  ):
    CanEq.Aux[L, R, cmp.Diff] =
    CanEq {
      (l, r) ⇒
        cmp(
          fn(l),
          r
        )
    }

  /**
   * Create a [[CanEq]] for two different types given a [[Cmp]] for one and a conversion function for the other into the
   * [[Cmp]]'s type
   */
  def withConversion[L, R](
    implicit
    cmp: Cmp[L],
    conv: R ⇒ L
  ):
    CanEq.Aux[L, R, cmp.Diff] =
    CanEq(cmp(_, _))

  /** Short-hands for applying a [[CanEq]] to two objects and returning the [[CanEq.Diff diff]], if any */
  trait dsl {
    def cmp[
      L,
      R
    ](
      l: L,
      r: R
    )(
      implicit
      cmp: CanEq[L, R]
    ):
      Option[cmp.Diff] =
      cmp(l, r)

    /**
     * This overload allows for coercing the second argument ("expected" value) to be the type of the first ("actual"
     * value), e.g.:
     *
     * {{{
     * def ids: Set[Int]
     *
     * // Diff `ids` against an empty Set
     * cmp(ids)(Set())
     *
     * // This won't compile, because the compiler will attempt to unify Set[Int] and Set[Nothing], and Set is invariant
     * cmp(ids, Set())
     * }}}
     *
     * @param cmp use a [[Wrapper]] here, otherwise this method has the same type after erasure as its overload above,
     *            and won't compile
     */
    def cmp[T, E](
      l: T
    )(
      r: T
    )(
      implicit
      cmp: Cmp.Wrapper[T, E]
    ):
      Option[cmp.Diff] =
      cmp(l, r)
  }

  object dsl extends dsl
}
