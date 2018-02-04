package org.hammerlab.test

import cats.Eq
import shapeless._

trait CanEq[-L, -R] {
  type Error
  def cmp(l: L, r: R): Option[Error]
  def apply(l: L, r: R): Option[Error] = cmp(l, r)
  def eqv(l: L, r: R): Boolean = cmp(l, r).isEmpty
}

trait LowPriCanEq
  extends Serializable {

  type Cmp[T] = CanEq[T, T]
  object Cmp {
    type Aux[T, E] = CanEq.Aux[T, T, E]
    def apply[T, E](fn: (T, T) ⇒ Option[E]): Cmp.Aux[T, E] = CanEq.instance[T, T, E](fn)
  }


  type Aux[T, U, E] = CanEq[T, U] { type Error = E }

  def instance[T, U, E](fn: (T, U) ⇒ Option[E]): CanEq.Aux[T, U, E] =
    new CanEq[T, U] {
      type Error = E
      override def cmp(t: T, u: U): Option[Error] = fn(t, u)
    }

  implicit def seqs[T, U](implicit
                          ce: CanEq[T, U]): CanEq.Aux[Seq[T], Seq[U], (Int, Option[ce.Error])] = {
    val iters: CanEq.Aux[Iterator[T], Iterator[U], (Int, Option[ce.Error])] = iterators[T, U](ce)
    instance[Seq[T], Seq[U], (Int, Option[ce.Error])](
      (s1, s2) ⇒
        iters(
          s1.iterator,
          s2.iterator
        )
    )
  }

  def iterators[T, U](implicit
                      ce: CanEq[T, U]): CanEq.Aux[Iterator[T], Iterator[U], (Int, Option[ce.Error])] =
    new CanEq[Iterator[T], Iterator[U]] {
      type Error = (Int, Option[ce.Error])
      override def cmp(t: Iterator[T], u: Iterator[U]): Option[Error] = cmp(0, t, u)
      def cmp(idx: Int, t: Iterator[T], u: Iterator[U]): Option[Error] =
        (t.hasNext, u.hasNext) match {
          case (true, true) ⇒
            ce
              .cmp(t.next, u.next)
              .map(e ⇒ idx → Some(e))
              .orElse(
                cmp(
                  idx + 1,
                  t,
                  u
                )
              )
          case (false, false) ⇒ None
          case _ ⇒ Some((idx, None))
        }
    }

  implicit def fromEq[T](implicit e: Eq[T]): Cmp[T] =
    instance(
      (t1, t2) ⇒
        if (e.eqv(t1, t2))
          None
        else
          Some(
            s"$t1 didn't match $t2"
          )
    )
}

trait MkCanEq
  extends LowPriCanEq {

  implicit def cclass[T, L <: HList, E <: Coproduct](implicit
                                                     gen: Generic.Aux[T, L],
                                                     listEq: Lazy[Cmp.Aux[L, E]]): Cmp[T] =
    Cmp[T, E](
      (l, r) ⇒
        listEq
          .value
          .cmp(
              gen.to(l),
              gen.to(r)
          )
    )

  implicit def cons[H, T <: HList, ET <: Coproduct](implicit
                                                    head: Lazy[Cmp[H]],
                                                    tail: Lazy[Cmp.Aux[T, ET]]): Cmp.Aux[H :: T, head.value.Error :+: ET] =
    Cmp[H :: T, head.value.Error :+: ET](
      (l, r) ⇒
        head
          .value
          .cmp(l.head, r.head)
          .map(e ⇒ Inl[head.value.Error, ET](e))
          .orElse(
            tail
              .value
              .cmp(l.tail, r.tail)
              .map(e ⇒ Inr[head.value.Error, ET](e))
          )
    )

  implicit val hnil: Cmp.Aux[HNil, CNil] = Cmp[HNil, CNil]((_, _) ⇒ None)


  implicit val cnil: Cmp.Aux[CNil, CNil] = Cmp[CNil, CNil]((_, _) ⇒ ???)

  implicit def ccons[H, T <: Coproduct, ET <: Coproduct](implicit
                                                         head: Lazy[Cmp[H]],
                                                         tail: Lazy[Cmp.Aux[T, ET]]): Cmp.Aux[H :+: T, head.value.Error :+: String :+: ET] =
    Cmp[H :+: T, head.value.Error :+: String :+: ET] {
      case (Inl(l), Inl(r)) ⇒
        head
          .value
          .cmp(l, r)
          .map(Inl(_))
      case (Inr(l), Inr(r)) ⇒
        tail
          .value
          .cmp(l, r)
          .map(e ⇒ Inr(Inr(e)))
      case (l, r) ⇒
        Some(Inr(Inl(s"Different types: $l $r")))
    }

  implicit def sealedTrait[T, C <: Coproduct, E <: Coproduct](implicit
                                                              gen: Generic.Aux[T, C],
                                                              coprodEq: Lazy[Cmp.Aux[C, E]]): Cmp[T] =
    Cmp[T, E](
      (t, u) ⇒
        coprodEq
          .value
          .cmp(
              gen.to(t),
              gen.to(u)
          )
    )

  def withConversion[T, U](implicit ce: CanEq[T, T], conv: U ⇒ T): CanEq[T, U] =
    instance((t, u) ⇒ ce.cmp(t, u))
}

object CanEq extends MkCanEq
