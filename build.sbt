name := "test-utils"
version := "1.2.4-SNAPSHOT"

addScala212

enableMacroParadise

deps ++= Seq(
  libs.value('commons_io),
  libs.value('paths).copy(revision = "1.1.1-SNAPSHOT"),
  scalatest.value,
  libs.value('scala_reflect)
)

// Don't inherit default test-deps from parent plugin.
testDeps := Seq()
