import scala.sys.process.Process

organization := "nl.surfnet"
name := "nsi-safnari"

//releaseSettings

lazy val mavenCommand = SettingKey[String]("maven-command", "Command to run maven")
lazy val deployDist = taskKey[File]("Deploy distribution using maven")

libraryDependencies ++= Seq(
  guice,
  ws,
  jdbc,
  evolutions,
  "org.scala-stm" %% "scala-stm" % "0.11.0",
  "org.postgresql" % "postgresql" % "42.3.1",
  "com.github.michaelahlers" % "play-json-zipper_2.11" % "1.2.0.23.1",
  //"org.specs2" %% "specs2-scalacheck" % "3.6.6" % "test",
  "org.specs2" %% "specs2-junit" % "4.3.5" % "test",
  "org.specs2" %% "specs2-matcher-extra" % "4.3.5" % "test",
  "com.typesafe.akka" %% "akka-testkit" % "2.5.26" % "test"
)

val gitHeadCommitSha = settingKey[String]("git HEAD SHA")

gitHeadCommitSha := Process("git rev-parse --short HEAD").lineStream.head

lazy val root = (project in file("."))
  .enablePlugins(PlayScala)
  .enablePlugins(BuildInfoPlugin)
  .settings(
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, gitHeadCommitSha),
    buildInfoPackage := "nl.surfnet.safnari"
  )
  .dependsOn(sub1 % "compile->compile;test->test")

//lazy val sub1 = RootProject( uri("git://github.com/BandwidthOnDemand/play-nsi-support.git") )
lazy val sub1 = RootProject( uri("../play-nsi-support") )

scalaVersion := "2.12.15"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xlint", "-Ywarn-unused", "-Ywarn-value-discard", "-target:jvm-1.8")

Test / javaOptions += "-Dconfig.file=conf/test.conf"

Test / testFrameworks := Seq(TestFrameworks.Specs2)

// Override Play! defaults to enable parallel test execution
Test / testOptions := Seq(Tests.Argument(TestFrameworks.Specs2, "junitxml", "console"))

//sourceGenerators in Compile <+= buildInfo

//PublishDist.publishSettings

// Disable ScalaDoc generation
Compile / doc /sources := Seq.empty
Compile / packageDoc / publishArtifact := false

Test / doc / sources := Seq.empty
Test / packageDoc / publishArtifact := false

// net.virtualvoid.sbt.graph.Plugin.graphSettings

// lazy val licenseText = settingKey[String]("Project license text.")

// licenseText := IO.read(baseDirectory.value / "LICENSE")

// headers := Map(
//   "scala" -> (
//     HeaderPattern.cStyleBlockComment,
//     licenseText.value.split("\n").map {
//       case ""   => " *"
//       case line => " * " ++ line
//     }.mkString("/*\n", "\n", "\n */\n")
//   )
// )

// publishTo := Some(Resolver.file("local-ivy", file("target")))
