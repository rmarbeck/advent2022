val scala3Version = "3.3.1"

lazy val advent2022 = (project
  .in(file("."))
  .settings(
    name := "advent of code 2022",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  ))
  .aggregate(projects*)
  .dependsOn(classPaths*)

val foundDirsWithBuildFile: Seq[String] = {
  val finder: PathFinder = file(".") ** "build.sbt"
  val result = finder.get.map(_.getParent).filter(_.contains("-12")).map(_.split('/')(1))
  result
}
val projects = foundDirsWithBuildFile.map(current => ProjectRef(file(s"./${current}"), s"advent${current.dropRight(3)}")).toSeq

val classPaths = projects.map(ClasspathDependency(_, None))

//lazy val `advent-08` = project
//lazy val `advent-07` = project
