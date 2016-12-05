package org.hammerlab.test.matchers.seqs

import org.hammerlab.test.matchers.utils.MatcherResultTest

class MapMatcherTest extends MatcherResultTest {

  def check[K: Ordering, V: Ordering](
      expected: (K, V)*
  )(
      actual: (K, V)*
  )(
      mismatchMessageRaw: String = ""
  ): Unit =
    checkResult(
      MapMatcher(expected.toMap).apply(actual.toMap),
      mismatchMessageRaw
    )

  test("empty match") {
    check[String, Int]()()()
  }

  test("single match") {
    check(
      "a" -> 1
    )(
      "a" -> 1
    )()
  }

  test("multiple match") {
    check(
      "a" -> 1,
      "b" -> 2,
      "c" -> 3
    )(
      "a" -> 1,
      "b" -> 2,
      "c" -> 3
    )()
  }

  test("single differing key") {
    check(
      "a" -> 1,
      "b" -> 2,
      "c" -> 3
    )(
      "a" -> 1,
      "b" -> 4,
      "c" -> 3
    )(
      """Sequences didn't match!
        |
        |Differing values:
        |	b: actual: 4, expected: 2
        |"""
    )
  }

  test("multiple differing keys") {
    check(
      "a" -> 1,
      "b" -> 2,
      "c" -> 3
    )(
      "a" -> 1,
      "b" -> 4,
      "c" -> 5
    )(
      """Sequences didn't match!
        |
        |Differing values:
        |	b: actual: 4, expected: 2
        |	c: actual: 5, expected: 3
        |"""
    )
  }

  test("empty vs single elem") {
    check()("a" -> 1)(
      """Sequences didn't match!
        |
        |Extra elems:
        |	a -> 1
        |"""
    )
  }

  test("single elem vs empty") {
    check("a" -> 1)()(
      """Sequences didn't match!
        |
        |Missing elems:
        |	a -> 1
        |"""
    )
  }

  test("missing and extra elems") {
    check(
      "a" -> 1,
      "b" -> 2,
      "c" -> 3
    )(
      "d" -> 4,
      "a" -> 1,
      "e" -> 5
    )(
      """Sequences didn't match!
        |
        |Extra elems:
        |	d -> 4
        |	e -> 5
        |
        |Missing elems:
        |	b -> 2
        |	c -> 3
        |"""
    )
  }

  test("two out of order elems") {
    check(
      "a" -> 1,
      "b" -> 2
    )(
      "b" -> 2,
      "a" -> 1
    )()
  }

  test("three out of order elems") {
    check(
      "a" -> 1,
      "b" -> 2,
      "a" -> 3
    )(
      "b" -> 2,
      "a" -> 1,
      "a" -> 3
    )()
  }
}
