package org.hammerlab.test.resources

import java.net.URL

object Url {
  def apply(path: String): URL = {
    Thread.currentThread().getContextClassLoader.getResource(path)
  }
}
