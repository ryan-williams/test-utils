package org.hammerlab.test

import java.nio.file.Files

import org.hammerlab.paths.Path
import org.hammerlab.test.files.TmpFiles
import org.hammerlab.test.matchers.files.{ HasDirMatchers, HasFileMatchers }
import org.hammerlab.test.resources.{ File, Url }

/**
 * Simple wrapper for common test-suite boilerplate.
 */
abstract class Suite
  extends hammerlab.Suite
     with TmpFiles
     with HasFileMatchers
     with HasDirMatchers {

  /**
   * Hacky helper for setting env variables
   */
  def setEnv(key: String, value: String) = {
    val field = System.getenv().getClass.getDeclaredField("m")
    field.setAccessible(true)
    val map = field.get(System.getenv()).asInstanceOf[java.util.Map[java.lang.String, java.lang.String]]
    map.put(key, value)
  }

  def path(name: String): Path = Path(Url(name).toURI)

  def resource(name: String): Path = File(name).path

  def fileCopy(path: Path, out: Path): Path = {
    val in = path.inputStream
    Files.copy(in, out)
    in.close()
    out
  }
}
