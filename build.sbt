name := "test-utils"
version := "1.2.0"

addScala212

enableMacroParadise

libraryDependencies ++= Seq(
  libs.value('commons_io),
  libs.value('scalatest),
  libs.value('scala_reflect)
)

// Don't inherit default test-deps from parent plugin.
testDeps := Seq()
