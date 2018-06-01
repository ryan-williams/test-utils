
default(
  subgroup("test"),
  // Don't inherit default test-deps from parent plugin.
  clearTestDeps,
  versions(
    math.tolerance → "1.0.0"
  )
)

lazy val base = project.settings(
  v"1.0.1",
  dep(
    // this should come from the suite.jvm classpath-dep below, but test-scoped dependencies don't transit as you'd
    // think/like
    math.tolerance tests,
    paths % "1.5.0"
  ),
  testDeps += scalatest
).dependsOn(
  `suite.jvm` andTest
)

lazy val suite = crossProject.settings(
  v"1.0.1",
  dep(
    cats,
    math.tolerance,
    scalatest,
    shapeless,
    "com.lihaoyi" ^^ "sourcecode" ^ "0.1.4"
  )
)
lazy val `suite.js`  = suite.js
lazy val `suite.jvm` = suite.jvm

lazy val `test-utils` = rootProject(
  base,
  `suite.js`, `suite.jvm`
)
