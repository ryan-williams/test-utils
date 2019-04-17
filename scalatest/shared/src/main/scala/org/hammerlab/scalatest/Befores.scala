package org.hammerlab.scalatest

import hammerlab.scalatest.Suite
import org.scalatest.{ BeforeAndAfterAll, BeforeAndAfterEach }

import scala.collection.mutable.ArrayBuffer

trait Befores
  extends BeforeAndAfterAll
    with BeforeAndAfterEach {
  self: Suite ⇒
  private val befores = ArrayBuffer[() ⇒ Unit]()

  def before(fn: ⇒ Unit): Unit = {
    befores += (() ⇒ fn)
  }

  final override def beforeEach(): Unit = {
    super.beforeEach()
    for { beforeFn ← befores } { beforeFn() }
  }
}
