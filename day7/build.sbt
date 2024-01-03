val scala3Version = "3.3.1"

lazy val adventday7 = project
  .in(file("."))
  .settings(
    name := "day7",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )