package hammerlab

import org.hammerlab.cmp.CanEq

package object cmp
  extends org.hammerlab.cmp.aliases {
  type tuple = org.hammerlab.cmp.tuples
  object tuple extends tuple
  type StartsWith = org.hammerlab.cmp.CanEq.StartsWith
   val StartsWith = org.hammerlab.cmp.CanEq.StartsWith
}
