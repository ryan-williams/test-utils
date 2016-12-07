package org.hammerlab.test.matchers.files

import java.io.File

import org.apache.commons.io.FileUtils
import org.hammerlab.test.resources.{File ⇒ Resource}
import org.scalatest.matchers.{ MatchResult, Matcher }

import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer

class DirMatcher(expectedDir: String) extends Matcher[String] {

  val expectedDirFile = Resource(expectedDir).file
  val expectedDirURI = expectedDirFile.toURI

  def recursiveListFiles(f: File): Set[File] =
    FileUtils.listFiles(f, null, true).toSet

  override def apply(actualDir: String): MatchResult = {

    val actualDirFile = new File(actualDir)
    val actualDirURI = actualDirFile.toURI

    val expectedFiles = recursiveListFiles(expectedDirFile).map(_.toURI).map(expectedDirURI.relativize).map(_.getPath)
    val actualFiles = recursiveListFiles(actualDirFile).map(_.toURI).map(actualDirURI.relativize).map(_.getPath)

    val extraFiles = actualFiles.diff(expectedFiles)
    val missingFiles = expectedFiles.diff(actualFiles)

    lazy val mismatchedFiles =
      for {
        file ← expectedFiles.intersect(actualFiles)
        expectedFile = new File(expectedDir, file).getPath
        actualFile = new File(actualDirFile, file).getPath
        matchResult = new FileMatcher(expectedFile).apply(actualFile)
        if !matchResult.matches
      } yield {
        expectedFile → matchResult.failureMessage
      }

    val errorLines = ArrayBuffer[String]()
    if (extraFiles.nonEmpty) {
      errorLines += "Extra files:"
      errorLines += ""
      errorLines ++= extraFiles.map("\t" + _)
      errorLines += ""
    }

    if (missingFiles.nonEmpty) {
      errorLines += "Missing files:"
      errorLines += ""
      errorLines ++= missingFiles.map("\t" + _)
      errorLines += ""
    }

    if (mismatchedFiles.nonEmpty) {
      errorLines += "Differing files:"
      errorLines += ""
      errorLines ++= (
        for { (file, msg) <- mismatchedFiles }
          yield
            s"\t$file:\n\t\t$msg\n"
      )
      errorLines += ""
    }

    MatchResult(
      extraFiles.isEmpty && missingFiles.isEmpty && mismatchedFiles.isEmpty,
      errorLines.mkString("\n"),
      s"$actualDirFile matched $expectedDir, but was supposed to not"
    )
  }
}

object DirMatcher {
  def dirMatch(actualDir: String): DirMatcher = new DirMatcher(actualDir)
}
