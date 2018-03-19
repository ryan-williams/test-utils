
default(
  group("org.hammerlab.test"),
  // Don't inherit default test-deps from parent plugin.
  clearTestDeps,
  versions(
    math.tolerance → "1.0.0"
  )
)

lazy val base = project.settings(
  v"1.0.1",
  dep(
    // this should come from the suiteJVM classpath-dep below, but test-scoped dependencies don't transit as you'd
    // think/like
    math.tolerance tests,
    paths % "1.5.0"
  ),
  testDeps += scalatest
).dependsOn(
  suiteJVM andTest
)

lazy val suite = crossProject.settings(
  r"1.0.0",
  dep(
    cats,
    math.tolerance,
    scalatest,
    shapeless
  )
)
lazy val suiteJS  = suite.js
lazy val suiteJVM = suite.jvm

lazy val macros = project.settings(
  group("org.hammerlab.macros"),
  name := "conversions",
  r"1.0.0",
  enableMacroParadise
).dependsOn(
  base.test
)

lazy val test_utils = rootProject(
  base,
  macros,
  suiteJS, suiteJVM
)
