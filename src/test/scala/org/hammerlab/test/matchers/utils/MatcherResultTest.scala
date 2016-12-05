package org.hammerlab.test.matchers.utils

import org.hammerlab.test.Suite
import org.scalatest.matchers.MatchResult

trait MatcherResultTest extends Suite {
  def checkResult(matchResult: MatchResult, expectedFailureMsgRaw: String): Unit = {
    val expectedFailureMsg = expectedFailureMsgRaw.stripMargin
    val shouldMatch = expectedFailureMsg.isEmpty

    val actualFailureMsg =
      if (matchResult.matches)
        ""
      else
        matchResult.failureMessage

      actualFailureMsg should be(expectedFailureMsg)
  }
}
