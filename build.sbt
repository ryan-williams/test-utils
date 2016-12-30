name := "test-utils"
version := "1.1.2"

addScala212

libraryDependencies ++= Seq(
  libs.value('commons_io),
  libs.value('scalatest)
)

// Don't inherit default test-deps from parent plugin.
testDeps := Seq()
