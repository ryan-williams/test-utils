name := "test-utils"
version := "1.2.4-SNAPSHOT"

addScala212

enableMacroParadise

deps ++= Seq(
  commons_io,
  paths % "1.1.1-SNAPSHOT",
  scalatest,
  scala_reflect
)

// Don't inherit default test-deps from parent plugin.
testDeps := Seq()
