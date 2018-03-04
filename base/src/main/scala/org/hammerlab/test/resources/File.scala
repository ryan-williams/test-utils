package org.hammerlab.test.resources

import java.net.URI

import org.hammerlab.paths.Path

/**
 * Wrapper for a test-resource file.
 */
class File private(val pathStr: String) {
  def path: Path = Path(pathStr)
}

object File {
  def apply(path: Path): File = apply(path.toString)
  def apply(path: String): File = new File(Url(path).getFile)
  implicit def fromString(path: String): File = apply(path)
  implicit def unmake(file: File): String = file.pathStr
  implicit def toPath(file: File): Path = Path(file)
  implicit def fromPath(path: Path): File = apply(path)
  implicit def fromURI(uri: URI): File = apply(Path(uri))
}
