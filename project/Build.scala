import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "nsi-safnari"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    "nl.surfnet.bod" % "bod-nsi" % "0.2.0-SNAPSHOT"
  )

  val main = play.Project(appName, appVersion, appDependencies).settings(
    resolvers ++= Seq(
        "SURFnet BoD Snapshots" at "http://atlas.dlp.surfnet.nl/nexus/content/repositories/public-snapshots",
        "SURFnet BoD Releases" at "http://atlas.dlp.surfnet.nl/nexus/content/repositories/public-releases"
    ),
    testFrameworks in Test := Seq(TestFrameworks.Specs2)
  )

}
