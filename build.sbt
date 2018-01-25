
default(
  group("org.hammerlab.test"),
  // Don't inherit default test-deps from parent plugin.
  testDeps := Seq(),
  v"1.0.0"
)

lazy val base = project.settings(
  dep(
    paths % "1.4.0"
  )
).dependsOn(
  suiteJVM
)

lazy val suite    = crossProject.settings(dep(scalatest))
lazy val suiteJS  = suite.js
lazy val suiteJVM = suite.jvm

lazy val macros = project.settings(
  group("org.hammerlab.macros"),
  name := "conversions",
  enableMacroParadise
).dependsOn(
  base test
)

lazy val test_utils = rootProject(base, macros, suiteJS, suiteJVM)
