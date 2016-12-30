package org.hammerlab.test.resources

import java.io.FileNotFoundException
import java.net.URL

object Url {
  def apply(path: String): URL = {
    Option(Thread.currentThread().getContextClassLoader.getResource(path)) match {
      case Some(url) ⇒ url
      case None ⇒
        throw new FileNotFoundException(s"Test resource not found: $path")
    }
  }
}
