package org.hammerlab.test.files

import java.nio.file.Files.{ createTempDirectory, createTempFile }

import org.hammerlab.paths.Path
import org.scalatest.{ BeforeAndAfterAll, Suite }

import scala.collection.mutable.ArrayBuffer

/**
 * Mix-in for tests that allows for creating temporary files and directories, and cleans them up before exiting.
 */
trait TmpFiles extends BeforeAndAfterAll {
  self: Suite ⇒

  val files = ArrayBuffer[Path]()
  val dirs = ArrayBuffer[Path]()

  def tmpFile(prefix: String = this.getClass.getSimpleName, suffix: String = ""): Path = {
    val f = Path(createTempFile(prefix, suffix))
    files += f
    f
  }

  def tmpDir(prefix: String = this.getClass.getSimpleName): Path = {
    val f = Path(createTempDirectory(prefix))
    dirs += f
    f
  }

  def tmpPath(prefix: String = this.getClass.getSimpleName,
              suffix: String = ""): Path =
    tmpDir() / (prefix + suffix)

  override def afterAll(): Unit = {
    super.afterAll()
    files.foreach(f ⇒ if (f.exists) f.delete())
    dirs.foreach(d ⇒ if (d.exists) d.delete(true))
  }
}
