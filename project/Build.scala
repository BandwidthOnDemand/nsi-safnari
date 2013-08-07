import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "nsi-safnari"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    jdbc,
    anorm,
    "nl.surfnet.bod" % "bod-nsi" % "0.2.3-SNAPSHOT" changing(),
    "org.scala-stm" %% "scala-stm" % "0.7",
    "com.h2database" % "h2" % "1.3.171",
    "org.scalacheck" %% "scalacheck" % "1.10.1" % "test",
    "com.typesafe.akka" %% "akka-testkit" % "2.1.0" % "test"
  )

  val main = play.Project(appName, appVersion, appDependencies).settings(
    scalaVersion := "2.10.2",
    scalacOptions := Seq("-deprecation", "-feature", "-unchecked", "-Xlint"),
    resolvers ++= Seq(
        "SURFnet BoD Snapshots" at "http://atlas.dlp.surfnet.nl/nexus/content/repositories/public-snapshots",
        "SURFnet BoD Releases" at "http://atlas.dlp.surfnet.nl/nexus/content/repositories/public-releases"
    ),
    testFrameworks in Test := Seq(TestFrameworks.Specs2),

    // Override Play! defaults to enable parallel test execution
    testOptions in Test := Seq(Tests.Argument(TestFrameworks.Specs2, "junitxml", "console"))
  )

}
