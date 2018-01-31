
default(
  group("org.hammerlab.test"),
  // Don't inherit default test-deps from parent plugin.
  testDeps := Nil,
  v"1.0.0"
)

lazy val base = project.settings(
  dep(
    paths % "1.4.0",
    // this should come from the suiteJVM classpath-dep below, but test-scoped dependencies don't transit as you'd
    // think/like
    "org.hammerlab.math" ^^ "syntax" ^ "1.0.0-SNAPSHOT" tests
  )
).dependsOn(
  suiteJVM andTest
)

lazy val suite = crossProject.settings(
  dep(
    cats,
    scalatest,
    shapeless,
    "org.hammerlab.math" ^^ "syntax" ^ "1.0.0-SNAPSHOT"
  )
)
lazy val suiteJS  = suite.js
lazy val suiteJVM = suite.jvm

lazy val macros = project.settings(
  group("org.hammerlab.macros"),
  name := "conversions",
  enableMacroParadise
).dependsOn(
  base test
)

lazy val test_utils = rootProject(
  base,
  macros,
  suiteJS, suiteJVM
)
