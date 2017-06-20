name := "test-utils"
version := "1.2.3"

addScala212

enableMacroParadise

deps ++= Seq(
  libs.value('commons_io),
  libs.value('paths),
  scalatest.value,
  libs.value('scala_reflect)
)

// Don't inherit default test-deps from parent plugin.
testDeps := Seq()
