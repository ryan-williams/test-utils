package org.hammerlab.test

import org.scalatest.{ FunSuite, Matchers }

// Simple wrapper for two commonly-subclassed scalatest interfaces.
class Suite extends FunSuite with Matchers
