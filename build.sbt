
default(
  subgroup("test"),
  v"1.0.4",
  `2.11`.add,
  // Don't inherit default test-deps from parent plugin.
  clearTestDeps,
  versions(
    math.tolerance → "1.0.0"
  )
)

lazy val base = project.settings(
  dep(
    // this should come from the suite.jvm classpath-dep below, but test-scoped dependencies don't transit as you'd
    // think/like
    math.tolerance tests,
    paths % "1.5.0"
  ),
  testDeps += scalatest
).dependsOn(
  suite.jvm andTest
)

lazy val suite = cross.settings(
  dep(
    cats,
    math.tolerance,
    scalatest,
    shapeless
  ),
  // java.nio.file.InvalidPathException: Malformed input or input contains unmappable characters: index/index-ε.html
  emptyDocJar
)
lazy val `suite-x` = suite.x

lazy val `test-utils` = root(
   base,
  `suite-x`
)
