package org.hammerlab.cmp

import scala.util.matching.Regex

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
