package org.hammerlab.test.resources

import org.hammerlab.paths.Path

trait PathUtil {
  implicit def stringToPath(path: String): Path = Path(File(path))
}
