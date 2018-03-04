
default(
  group("org.hammerlab.test"),
  v"1.0.0",
  // Don't inherit default test-deps from parent plugin.
  clearTestDeps,
  versions(
    math.tolerance → "1.0.0"
  ),
  resolvers += Resolver.sonatypeRepo("orghammerlab-1405")
)

lazy val base = project.settings(
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
  enableMacroParadise
).dependsOn(
  base.test
)

lazy val test_utils = rootProject(
  base,
  macros,
  suiteJS, suiteJVM
)
