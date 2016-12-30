package org.hammerlab.test.resources

import java.io.{ File ⇒ JFile }
import java.nio.file.{ Files, Paths }

import scala.io.Source

/**
 * Wrapper for a test-resource file.
 */
class File private(val path: String) extends AnyVal {
  def read: String =
    Source.fromFile(path).mkString

  def readBytes: Array[Byte] =
    Files.readAllBytes(Paths.get(path))

  def file: JFile = new JFile(path)
}

object File {
  def apply(path: String): File = new File(Url(path).getFile)
  implicit def unmake(file: File): String = file.path
}
