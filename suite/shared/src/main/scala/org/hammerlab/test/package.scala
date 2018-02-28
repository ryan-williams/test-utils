package org.hammerlab

import org.hammerlab.cmp.CanEq
import org.hammerlab.test.matchers.lines.HasMatcher

package object test
  extends HasMatcher {
  type Cmp[T] = CanEq.Cmp[T]
  val Cmp = CanEq.Cmp
}
