import hammerlab._

default(
  subgroup("test"),
  v"2.0.0",
  // Don't inherit default test-deps from parent plugin.
  addTestLib := false,
  versions(
    math.tolerance → "1.0.0",
    types → "1.6.0".snapshot,
  )
)

import Test.{ autoImport ⇒ tests }

lazy val base =
  project
    .settings(
      dep(
        // this should come from the suite.jvm classpath-dep below, but test-scoped dependencies don't transit as you'd
        // think/like
        math.tolerance tests,
        paths % "1.5.0",
        tests.scalatest compile
      ),
    ).dependsOn(
      cmp jvm,
      scalatest.jvm andTest
    )

lazy val cmp = cross.settings(
  dep(
    cats,
    math.tolerance,
    shapeless,
    types
  )
)
lazy val `cmp-x` = cmp.x

lazy val scalatest = cross.settings(
  dep(
    tests.scalatest compile
  )
)
.dependsOn(cmp)
lazy val `scalatest-x` = scalatest.x

lazy val utest = cross.settings(
  dep(
    tests.utest compile
  )
)
.dependsOn(cmp)
lazy val `utest-x` = utest.x

lazy val `test-utils` = root(
        base   ,
        `cmp-x`,
  `scalatest-x`,
      `utest-x`,
)
