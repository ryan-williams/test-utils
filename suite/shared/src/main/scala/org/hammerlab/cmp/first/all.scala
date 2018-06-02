package org.hammerlab.cmp.first

trait all
  extends Collections
     with CaseClass
     with SealedTrait {
  object collections extends Collections
  object   caseclass extends   CaseClass
  object sealedtrait extends SealedTrait
}
