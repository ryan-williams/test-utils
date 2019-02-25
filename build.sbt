import hammerlab._

default(
  subgroup("test"),
  v"2.0.0",
  // Don't inherit default test-deps from parent plugin.
  addTestLib := false,
  versions(
    math.tolerance → "1.0.0",
    types → "1.5.0",
  )
)

lazy val base = cross.settings(
  dep(
    // this should come from the suite.jvm classpath-dep below, but test-scoped dependencies don't transit as you'd
    // think/like
    math.tolerance tests,
    "org.lasersonlab" ^^ "uri" ^ "0.1.0".snapshot
  ),
  testDeps += scalatest
).dependsOn(
  cmp,
  suite andTest
)
lazy val `base-x` = base.x

lazy val cmp = cross.settings(
  dep(
    cats,
    math.tolerance,
    shapeless,
    types
  )
)
lazy val `cmp-x` = cmp.x

lazy val suite = cross.settings(
  dep(
    cats,
    math.tolerance,
    scalatest compile,
    shapeless
  ),
  // java.nio.file.InvalidPathException: Malformed input or input contains unmappable characters: index/index-ε.html
  emptyDocJar
)
.dependsOn(
  cmp
)
lazy val `suite-x` = suite.x

lazy val `test-utils` = root(
   `base-x`,
    `cmp-x`,
  `suite-x`,
)
