package org.hammerlab.cmp.first

import org.hammerlab.cmp.CanEq

package object collections {
  /**
   * Sum-type for the first difference found between two collections
   *
   * @tparam L type of elements in the LHS collection; returned as a [[LeftOnly]] if e.g. the RHS is a prefix of the LHS
   *           (so the first differing index is just beyond the length of the RHS)
   * @tparam R type of elements in the RHS collection; returned as a [[RightOnly]] if e.g. the LHS is a prefix of the
   *           RHS (so the first differing index is just beyond the length of the LHS)
   * @tparam B "both": diff-type returned by a [[CanEq]] that compares an [[L]] and [[R]] and finds a difference before
   *           the end of either collection; this gets wrapped in and returned as a [[Diff]]
   */
  sealed trait  ElemDiff[+L, +R, +B]       extends Product with Serializable
  sealed trait  ElemOnly[+L, +R    ]       extends ElemDiff[L, R, Nothing]
    case class  LeftOnly[ L        ](l: L) extends ElemOnly[L, Nothing]
    case class RightOnly[     R    ](r: R) extends ElemOnly[Nothing, R]
    case class      Diff[         B](b: B) extends ElemDiff[Nothing, Nothing, B]

  /**
   * Diff-type for a [[CanEq]] that returns an index/"key" [[Key]] at which two collections have a different value
   * (expressed as an [[ElemDiff]]
   */
  type DiffT[Key, Left, Right, Diff] = (Key, ElemDiff[Left, Right, Diff])

  /**
   * Collection-diff-type specialized for [[Int]]-indices
   */
  type IndexedDiff[Left, Right, Diff] = DiffT[Int, Left, Right, Diff]
}
