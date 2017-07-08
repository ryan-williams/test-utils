package org.hammerlab.test.matchers.lines

import org.hammerlab.test.{ Suite, linesMatch }

class LinesMatchingTest
  extends Suite {
  test("plain strings") {
    "abc def\tghi" should linesMatch("abc def\tghi")
    "" should linesMatch("")

    "" should not(linesMatch("a"))
    "a" should not(linesMatch("b"))
    "a" should not(linesMatch(""))
  }

  test("multiple lines") {
    """abc def	ghi
      |jkl
      |
      |m
      |
      |"""
      .stripMargin should linesMatch(
      "abc def\tghi",
      "jkl",
      "",
      "m",
      "",
      ""
    )

    """abc def	ghi
      |jkl
      |
      |
      |m
      |"""
    .stripMargin should linesMatch(
      "abc def\tghi",
      "jkl",
      "",
      "",
      "m",
      ""
    )
  }

  import Line._

  test("wildcards") {
    """abc 1
      |123 def
      |ggg45hhh
      |77 ,,123  88
      |"""
    .stripMargin should linesMatch(
      "abc " ++ LineNumber,
      LineNumber ++ " def",
      "ggg" ++ LineNumber ++ "hhh",
      LineNumber ++ Chars(",123 ") ++ LineNumber,
      ""
    )

    """abc 1
      |123 def
      |ggg45hhh
      |77 ,,1243  88
      |"""
    .stripMargin should not(
      linesMatch(
        "abc " ++ LineNumber,
        LineNumber ++ " def",
        "ggg" ++ LineNumber ++ "hhh",
        LineNumber ++ Chars(",123 ") ++ LineNumber,
        ""
      )
    )
  }
}
