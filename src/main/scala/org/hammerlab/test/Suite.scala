package org.hammerlab.test

import org.scalactic.ConversionCheckedTripleEquals
import org.scalatest.{ FunSuite, Matchers }

// Simple wrapper for common test-suite boilerplate.
class Suite
  extends FunSuite
    with Matchers
    with ConversionCheckedTripleEquals
    with implicits.Templates
