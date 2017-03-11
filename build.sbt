name := "test-utils"
version := "1.2.0-SNAPSHOT"

addScala212

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

libraryDependencies ++= Seq(
  libs.value('commons_io),
  libs.value('scalatest),
  libs.value('scala_reflect)
)

// Don't inherit default test-deps from parent plugin.
testDeps := Seq()
