package org.hammerlab.cmp

import hammerlab.option._

import scala.util.matching.Regex

trait Regexs
  extends first.collections.all {
  implicit val stringCanEqRegex: CanEq.Aux[String, Regex, (String, Regex)] =
    new CanEq[String, Regex] {
      type Δ = (String, Regex)
      def cmp(l: String, r: Regex): ?[Δ] =
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
      type Δ = (String, String)
      def cmp(l: String, r: StartsWith): ?[Δ] =
        if (l.startsWith(r.str))
          None
        else
          Some((l, r.str))
    }
}
