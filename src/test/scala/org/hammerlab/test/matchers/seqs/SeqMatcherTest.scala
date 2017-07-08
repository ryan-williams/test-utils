package org.hammerlab.test.matchers.seqs

import org.hammerlab.test.matchers.utils.MatcherResultTest

import scala.reflect.ClassTag

class SeqMatcherTest
  extends MatcherResultTest {

  def check[T: ClassTag](
      expected: T*
  )(
      actual: T*
  )(
      mismatchMessageRaw: String = ""
  ): Unit =
    checkResult(
      SeqMatcher(expected).apply(actual),
      mismatchMessageRaw
    )

  test("empty match") {
    check[String]()()()
  }

  test("single match") {
    check(
      "a"
    )(
      "a"
    )()
  }

  test("multiple match") {
    check(
      "a",
      "b",
      "c"
    )(
      "a",
      "b",
      "c"
    )()
  }

  test("one extra elem") {
    check(
      "a",
      "b"
    )(
      "a",
      "b",
      "c"
    )(
      """Sequences didn't match!
        |
        |Extra elems:
        |	c
        |"""
    )
  }

  test("one missing elem") {
    check(
      "a",
      "b",
      "c"
    )(
      "a",
      "b"
    )(
      """Sequences didn't match!
        |
        |Missing elems:
        |	c
        |"""
    )
  }

  test("three out of order elems") {
    check(
      "a",
      "b",
      "c"
    )(
      "a",
      "c",
      "b"
    )(
      """Sequences didn't match!
        |
        |Elements out of order:
        |
        |Expected:
        |	a
        |	b
        |	c
        |
        |Actual:
        |	a
        |	c
        |	b
        |"""
    )
  }
}

class ArrMatcherTest
  extends SeqMatcherTest {
  override def check[T: ClassTag](
      expected: T*
  )(
      actual: T*
  )(
      mismatchMessageRaw: String = ""
  ): Unit =
    checkResult(
      ArrMatcher(expected).apply(actual.toArray),
      mismatchMessageRaw
    )
}
