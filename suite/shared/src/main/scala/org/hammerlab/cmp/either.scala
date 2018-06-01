package org.hammerlab.cmp

import org.hammerlab.test.Cmp

trait either {
//  sealed trait EitherDiff[L, R, LDiff, RDiff]
//  case class     Lefts[L, R, LDiff, RDiff](diff: LDiff) extends EitherDiff[L, R, LDiff, RDiff]
//  case class    Rights[L, R, LDiff, RDiff](diff: RDiff) extends EitherDiff[L, R, LDiff, RDiff]
//  case class LeftRight[L, R, LDiff, RDiff]( l: L, r: R) extends EitherDiff[L, R, LDiff, RDiff]
//  case class RightLeft[L, R, LDiff, RDiff]( l: R, r: L) extends EitherDiff[L, R, LDiff, RDiff]

//  implicit def cmpEither[L, R](
//    implicit
//    cmpL: Cmp[L],
//    cmpR: Cmp[R]
//  ):
//    Cmp.Aux[
//      Either[L, R],
//      EitherDiff[
//        L,
//        R,
//        cmpL.Error,
//        cmpR.Error
//        ]
//      ] =
//    Cmp {
//      case ( Left(l),  Left(r)) ⇒ cmpL(l, r).map(d ⇒  Lefts(d))
//      case (Right(l), Right(r)) ⇒ cmpR(l, r).map(d ⇒ Rights(d))
//      case ( Left(l), Right(r)) ⇒ Some(LeftRight(l, r))
//      case (Right(l),  Left(r)) ⇒ Some(RightLeft(l, r))
//    }

}
