import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "nsi-safnari"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    "nl.surfnet.bod" % "bod-nsi" % "0.2.0-SNAPSHOT",
    "org.scala-stm" %% "scala-stm" % "0.7",
    "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"
  )

  val main = play.Project(appName, appVersion, appDependencies).settings(
    scalaVersion := "2.10.1",
    scalacOptions := Seq("-deprecation", "-feature", "-unchecked", "-Xlint"),
    resolvers ++= Seq(
        "SURFnet BoD Snapshots" at "http://atlas.dlp.surfnet.nl/nexus/content/repositories/public-snapshots",
        "SURFnet BoD Releases" at "http://atlas.dlp.surfnet.nl/nexus/content/repositories/public-releases"
    ),
    testFrameworks in Test := Seq(TestFrameworks.Specs2)
  )

}
