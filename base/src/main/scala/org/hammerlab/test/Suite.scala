package org.hammerlab.test

import java.nio.file.Files
import java.{ lang ⇒ jl }

import org.hammerlab.paths.Path
import org.hammerlab.test.files.TmpFiles
import org.hammerlab.test.resources.Url
import org.scalactic.{ CanEqual, ConversionCheckedTripleEquals }

/**
 * Simple wrapper for common test-suite boilerplate.
 */
abstract class Suite
  extends org.hammerlab.Suite
    with ConversionCheckedTripleEquals
    with TmpFiles {

  def obviousEquality[L, R]: CanEqual[L, R] =
    new CanEqual[L, R] {
      override def areEqual(a: L, b: R): Boolean = a == b
    }

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

  def fileCopy(path: Path, out: Path): Path = {
    val in = path.inputStream
    Files.copy(in, out)
    in.close()
    out
  }

  // Some implicits to allow trivial conversions for type-safe equality checking with ===.
  implicit val intLongEqual = obviousEquality[Int, Long]
  implicit val longIntEqual = obviousEquality[Long, Int]
  implicit val jlongIntEqual = obviousEquality[jl.Long, Int]
  implicit val jlongLongEqual = obviousEquality[jl.Long, Long]
  implicit val longJLongEqual = obviousEquality[Long, jl.Long]
  implicit val integerIntEqual = obviousEquality[Integer, Int]
  implicit val jboolBoolEqual = obviousEquality[jl.Boolean, Boolean]
  implicit val doubleFloatEqual = obviousEquality[jl.Double, Float]
  implicit val jfloatFloatEqual = obviousEquality[jl.Float, Float]
}
