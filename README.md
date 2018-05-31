# test-utils

[![Build Status](https://travis-ci.org/hammerlab/test-utils.svg?branch=master)](https://travis-ci.org/hammerlab/test-utils)
[![codecov](https://codecov.io/gh/hammerlab/test-utils/branch/master/graph/badge.svg)](https://codecov.io/gh/hammerlab/test-utils)
[![Maven Central](https://img.shields.io/maven-central/v/org.hammerlab.test/base_2.12.svg?maxAge=600)](http://search.maven.org/#search%7Cga%7C1%7Cg%3Aorg.hammerlab.test)

- [`suite`](suite): test suites, custom matchers, and more for the JVM and JS
- [`base`](base): JVM-specific extensions to [`suite`](suite) that adds filesystem-utilities (e.g. creating temporary files/directories)

## Get

```scala
libraryDependencies += "org.hammerlab.test" %%% "suite" % "1.0.1"
```

or

```scala
libraryDependencies += "org.hammerlab.test" %% "base" % "1.0.1"
```

## Use

- [`Suite`](https://github.com/hammerlab/test-utils/blob/master/src/main/scala/org/hammerlab/test/Suite.scala): wrapper for ScalaTest `FunSuite with Matchers`.
- [`TmpFiles`](https://github.com/hammerlab/test-utils/blob/master/src/main/scala/org/hammerlab/test/files/TmpFiles.scala): create (and automatically garbage-collect) temporary files and directories.
- [`matchers`](https://github.com/hammerlab/test-utils/tree/master/src/main/scala/org/hammerlab/test/matchers): various ScalaTest `Matcher` implementations:
  - [`files`](https://github.com/hammerlab/test-utils/tree/master/src/main/scala/org/hammerlab/test/matchers/files): file- and directory-equality `Matcher`s:
    - [`FileMatcher`](https://github.com/hammerlab/test-utils/blob/master/src/main/scala/org/hammerlab/test/matchers/files/FileMatcher.scala): assert that two files' contents are equal, where the "expected" path is interpreted as being a path to a test-resource, and the "actual" path is absolute (e.g. a temporary file that was written via `TmpFiles` above):

      ```scala
      import org.hammerlab.test.matchers.files.FileMatcher.fileMatch
      actualPath should fileMatch(expectedPath)
      ```

    - [`DirMatcher`](https://github.com/hammerlab/test- utils/blob/master/src/main/scala/org/hammerlab/test/matchers/files/DirMatcher.scala): similar to the above, but verifying two directories' contents, highlighting missing, extra, and differing files.

      ```scala
      import org.hammerlab.test.matchers.files.DirMatcher.dirMatch
      actualPath should dirMatch(expectedPath)
      ```
      
  - [`seqs`](https://github.com/hammerlab/test-utils/tree/master/src/main/scala/org/hammerlab/test/matchers/seqs): collection-related `Matcher`s:
    - [`SeqMatcher`](https://github.com/hammerlab/test-utils/blob/master/src/main/scala/org/hammerlab/test/matchers/seqs/SeqMatcher.scala)/[`SetMatcher`](https://github.com/hammerlab/test-utils/blob/master/src/main/scala/org/hammerlab/test/matchers/seqs/SetMatcher.scala): compare two `Seq`s, highlighting extra/missing elements.
    - [`PairSeqMatcher`](https://github.com/hammerlab/test-utils/blob/master/src/main/scala/org/hammerlab/test/matchers/seqs/PairSeqMatcher.scala)/[`MapMatcher`](https://github.com/hammerlab/test-utils/blob/master/src/main/scala/org/hammerlab/test/matchers/seqs/MapMatcher.scala): compare two collections of key-value pairs, highlighting missing/extra elements as well as keys whose values differ.
- [`LazyAssert`](https://github.com/hammerlab/test-utils/blob/master/src/main/scala/org/hammerlab/test/matchers/LazyAssert.scala): an `assert` wrapper whose failure-message is evaluated lazily; useful in case it is expensive to compute (e.g. materializes/indexes data to provide context for debugging).
- [`version.Util`](https://github.com/hammerlab/test-utils/blob/eb200189167e9daba369ef8adcf914bd2552ad96/src/main/scala/org/hammerlab/test/version/Util.scala): test the Scala version, at runtime.
