name := "test-utils"
version := "1.4.0"

addScala212

enableMacroParadise

deps ++= Seq(
  commons_io,
  paths % "1.3.0",
  scalatest,
  scala_reflect
)

// Don't inherit default test-deps from parent plugin.
testDeps := Seq()
