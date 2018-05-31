
default(
  subgroup("test"),
  // Don't inherit default test-deps from parent plugin.
  clearTestDeps,
  versions(
    math.tolerance → "1.0.0"
  )
)

lazy val base = project.settings(
  r"1.0.1",
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
  r"1.0.1",
  dep(
    cats,
    math.tolerance,
    scalatest,
    shapeless
  )
)
lazy val `suite.js`  = suite.js
lazy val `suite.jvm` = suite.jvm

lazy val snippets =
  project
    .settings(
      group("org.hammerlab.docs"),
      v"1.0.0",
      enableMacroParadise,
      // macros mess up doc-generation
      skipDoc,
      dep(
        hammerlab.io % "5.0.1" snapshot,
        scalatags
      )
    )
    .enablePlugins(
      ScalaJSPlugin
    )
    .dependsOn(
      `suite.js` andTest
    )

lazy val `test-utils` = rootProject(
  base,
  snippets,
  `suite.js`, `suite.jvm`
)
