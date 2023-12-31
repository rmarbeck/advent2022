import sbt._
import Keys._

object Common {
  
  val scala3Version = "3.3.1"
  val settings: Seq[Setting[_]] = Seq(
    organization := "fr.hometime",
    version := "0.9-SNAPSHOT",
    scalaVersion := scala3Version
  )

  val metaDependency = "org.scalameta" %% "munit" % "0.7.29" % Test
}
