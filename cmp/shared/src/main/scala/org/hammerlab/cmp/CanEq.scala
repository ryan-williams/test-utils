package org.hammerlab.cmp

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
