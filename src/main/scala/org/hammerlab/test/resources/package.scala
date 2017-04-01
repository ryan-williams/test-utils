package org.hammerlab.test

import org.hammerlab.paths.Path

package object resources {
  implicit def stringToTestPath(pathStr: String): Path = File(pathStr)
}
