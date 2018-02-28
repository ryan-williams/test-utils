package org.hammerlab.test.matchers.files

import java.io.{ ByteArrayOutputStream, PrintWriter }

import org.hammerlab.paths.Path
import org.hammerlab.test.resources.File
import org.scalatest.matchers.{ MatchResult, Matcher }

class DirMatcher(expected: Path)
  extends Matcher[Path] {

  def dirMap(dir: Path): Map[Path, Path] =
    dir
      .walk
      .map(
        path ⇒
          Path(dir.relativize(path)) →
            path
      )
      .toMap

  def err(line: String = "")(implicit pw: PrintWriter): Unit = {
    pw println line
  }

  def err(lines: Seq[String])(implicit pw: PrintWriter): Unit = {
    lines foreach (pw println)
  }

  override def apply(actual: Path): MatchResult = {

    val expectedPathsMap = dirMap(expected)
    val actualPathsMap = dirMap(actual)

    val expectedPaths = expectedPathsMap.keySet
    val actualPaths = actualPathsMap.keySet

    val extraPaths = actualPaths.diff(expectedPaths)
    val missingPaths = expectedPaths.diff(actualPaths)

    lazy val mismatchedPaths =
      for {
        path ← expectedPaths.intersect(actualPaths)
        expectedPath = expectedPathsMap(path)
        actualPath = actualPathsMap(path)
        if !expectedPath.isDirectory && !actualPath.isDirectory
        matchResult = new FileMatcher(expectedPath).apply(actualPath)
        if !matchResult.matches
      } yield {
        path → matchResult.failureMessage
      }

    val baos = new ByteArrayOutputStream
    implicit val pw = new PrintWriter(baos)

    implicit val pathOrd = Ordering.by[Path, String](_.toString)

    if (extraPaths.nonEmpty) {
      err("Extra files:")
      err()
      err(extraPaths.toArray.sorted.map("\t" + _))
      err()
    }

    if (missingPaths.nonEmpty) {
      err("Missing files:")
      err()
      err(missingPaths.toArray.sorted.map("\t" + _))
      err()
    }

    if (mismatchedPaths.nonEmpty) {
      err("Differing files:")
      err()
      err(
        for {
          (path, msg) <- mismatchedPaths.toArray.sorted
        } yield
          s"\t$path:\n\t\t$msg\n"
      )
    }

    pw.close()

    lazy val errors = new String(baos.toByteArray)

    MatchResult(
      extraPaths.isEmpty && missingPaths.isEmpty && mismatchedPaths.isEmpty,
      errors,
      s"$actual matched $expected, but was supposed to not"
    )
  }
}

trait HasDirMatchers {
  def dirMatch(expectedDir: Path): DirMatcher = new DirMatcher(expectedDir)
  def dirMatch(expectedDir: File): DirMatcher = new DirMatcher(expectedDir)
}

object DirMatcher extends HasDirMatchers
