package hammerlab

package object cmp {
  type tuple = org.hammerlab.cmp.tuples
  object tuple extends tuple
  type StartsWith = org.hammerlab.cmp.CanEq.StartsWith
   val StartsWith = org.hammerlab.cmp.CanEq.StartsWith
}
