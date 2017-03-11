name := "test-utils"
version := "1.1.6"

addScala212

libraryDependencies ++= Seq(
  libs.value('commons_io),
  libs.value('scalatest),
  libs.value('scala_reflect)
)

// Don't inherit default test-deps from parent plugin.
testDeps := Seq()
