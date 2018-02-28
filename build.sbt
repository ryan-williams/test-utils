
default(
  group("org.hammerlab.test"),
  v"1.0.0",
  // Don't inherit default test-deps from parent plugin.
  clearTestDeps,
  versions(
    hammerlab("math", "syntax") → "1.0.0"
  )
)

lazy val base = project.settings(
  dep(
    paths % "1.5.0",
    // this should come from the suiteJVM classpath-dep below, but test-scoped dependencies don't transit as you'd
    // think/like
    hammerlab("math", "syntax") tests
  ),
  testDeps += scalatest
).dependsOn(
  suiteJVM andTest
)

lazy val suite = crossProject.settings(
  dep(
    cats,
    scalatest,
    shapeless,
    hammerlab("math", "syntax")
  )
)
lazy val suiteJS  = suite.js
lazy val suiteJVM = suite.jvm

lazy val macros = project.settings(
  group("org.hammerlab.macros"),
  name := "conversions",
  enableMacroParadise
).dependsOn(
  base.test
)

lazy val test_utils = rootProject(
  base,
  macros,
  suiteJS, suiteJVM
)
