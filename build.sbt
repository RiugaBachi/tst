val scala3Version = "3.4.2"

lazy val commonSettings = Seq(
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test,
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.15.4" % "test",
    libraryDependencies += "org.scalameta" %% "munit-scalacheck" % "0.7.29" % Test
  )

lazy val global = project
  .in(file("."))
  .settings(commonSettings: _*)
  .settings(
    name := "tst-takehome"
  )
