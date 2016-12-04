name := "test-utils"
version := "1.0.0-SNAPSHOT"
libraryDependencies ++= Seq(
  libraries.value('scalatest),
  "commons-io" % "commons-io" % "2.4"
)
