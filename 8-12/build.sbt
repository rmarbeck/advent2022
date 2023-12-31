val scala3Version = "3.3.1"

lazy val advent8  = project
  .in(file("."))
  .settings(
    name := "8-12",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )
