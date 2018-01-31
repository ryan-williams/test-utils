package org.hammerlab.test

import org.hammerlab.Suite
import org.scalatest.{ BeforeAndAfterAll, BeforeAndAfterEach }

import scala.collection.mutable.ArrayBuffer

trait Afters
  extends BeforeAndAfterAll
    with BeforeAndAfterEach {
  self: Suite ⇒
  private val afters = ArrayBuffer[() ⇒ Unit]()

  def after(fn: ⇒ Unit): Unit = {
    afters += (() ⇒ fn)
  }

  final override def afterEach(): Unit = {
    super.afterEach()
    for { afterFn ← afters } { afterFn() }
  }
}
