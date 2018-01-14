name := "test-utils"
r"1.5.1"

enableMacroParadise

dep(
  commons_io,
  paths % "1.3.1",
  scalatest,
  scala_reflect
)

// Don't inherit default test-deps from parent plugin.
testDeps := Seq()
