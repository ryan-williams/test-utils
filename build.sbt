name := "test-utils"
version := "1.2.2"

addScala212

enableMacroParadise

deps ++= Seq(
  libs.value('commons_io),
  libs.value('paths),
  libs.value('scalatest),
  libs.value('scala_reflect)
)

// Don't inherit default test-deps from parent plugin.
testDeps := Seq()
