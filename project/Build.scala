import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "nsi-safnari"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    jdbc,
    anorm,
    "nl.surfnet.bod" % "bod-nsi" % "0.2.4",
    "org.scala-stm" %% "scala-stm" % "0.7",
    "org.postgresql" % "postgresql" % "9.2-1003-jdbc4",
    "org.scalacheck" %% "scalacheck" % "1.10.1" % "test",
    "com.typesafe.akka" %% "akka-testkit" % "2.2.0" % "test"
  )

  lazy val mavenCommand = settingKey[String]("Command to run maven")
  lazy val deployDist = taskKey[File]("Deploy distribution using maven")

  val main = play.Project(appName, appVersion, appDependencies).settings(
    organization := "nl.surfnet.bod",
    scalaVersion := "2.10.3",
    scalacOptions := Seq("-deprecation", "-feature", "-unchecked", "-Xlint"),
    resolvers ++= Seq(
        "SURFnet BoD Snapshots" at "http://atlas.dlp.surfnet.nl/nexus/content/repositories/public-snapshots",
        "SURFnet BoD Releases" at "http://atlas.dlp.surfnet.nl/nexus/content/repositories/public-releases"
    ),
    javaOptions in Test += "-Dconfig.file=conf/test.conf",
    testFrameworks in Test := Seq(TestFrameworks.Specs2),

    // Override Play! defaults to enable parallel test execution
    testOptions in Test := Seq(Tests.Argument(TestFrameworks.Specs2, "junitxml", "console")),

    mavenCommand := "mvn",

    deployDist := {
      val distFile = com.typesafe.sbt.packager.Keys.dist.value
      val nexus = "http://atlas.dlp.surfnet.nl/nexus/content/repositories/"
      val version = Keys.version.value
      val (repoId, repoUrl) = if (version.trim.endsWith("-SNAPSHOT")) ("surfnet-snapshots", nexus + "snapshots") else ("surfnet-releases", nexus + "releases")
      val groupId = organization.value
      val artifactId = artifact.value.name
      val maven = mavenCommand.value
      val command = Seq(
        maven, "-B", "deploy:deploy-file",
        s"-DrepositoryId=$repoId", s"-Durl=$repoUrl", s"-Dfile=$distFile",
        s"-DgroupId=$groupId", s"-DartifactId=$artifactId", s"-Dversion=$version", "-Dpackaging=zip")

      println(s"Deploying $distFile to $repoId at $repoUrl using command:\n ${command.mkString(" ")}")
      Process(command).lines.foreach(println)
      distFile
    }
  )
}
