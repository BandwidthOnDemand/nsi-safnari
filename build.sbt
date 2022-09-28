import scala.sys.process.Process

organization := "nl.surfnet"
name := "nsi-safnari"

githubTokenSource := TokenSource.GitConfig("github.token") || TokenSource.Environment("GITHUB_USERTOKEN") || TokenSource.Environment("GITHUB_TOKEN")

//releaseSettings

lazy val mavenCommand = SettingKey[String]("maven-command", "Command to run maven")
lazy val deployDist = taskKey[File]("Deploy distribution using maven")

val playVersion = "2.7.9"

libraryDependencies ++= Seq(
  guice,
  ws,
  jdbc,
  evolutions,
  "org.scala-stm" %% "scala-stm" % "0.11.0",
  "org.postgresql" % "postgresql" % "42.5.0",
  "org.specs2" %% "specs2-junit" % "4.13.0" % "test",
  "org.specs2" %% "specs2-matcher-extra" % "4.13.0" % "test",
  "org.specs2" %% "specs2-scalacheck" % "4.13.0" % "test",
  "com.typesafe.akka" %% "akka-testkit" % "2.5.26" % "test",
  "com.typesafe.play" %% "play-test" % playVersion % "test",
  "com.typesafe.play" %% "play-specs2" % playVersion % "test",
  "nl.surfnet" % "play-nsi-support_2.13" % "2.1.6",
  "nl.surfnet" % "play-nsi-support_2.13" % "2.1.6" % "test" classifier "tests",
)

val gitHeadCommitSha = settingKey[String]("git HEAD SHA")

gitHeadCommitSha := Process("git rev-parse --short HEAD").lineStream.head

lazy val root = (project in file("."))
  .enablePlugins(PlayScala)
  .enablePlugins(BuildInfoPlugin)
  .settings(
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, gitHeadCommitSha),
    buildInfoPackage := "nl.surfnet.safnari",
    exportJars := true,
  )
//  .enablePlugins(PlayScala, PlayNettyServer)
//  .disablePlugins(PlayAkkaHttpServer)

scalaVersion := "2.13.7"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xlint", "-Ywarn-unused", "-Ywarn-value-discard", "-target:jvm-1.8")

Test / javaOptions += "-Dconfig.file=conf/test.conf"

Test / testFrameworks := Seq(TestFrameworks.Specs2)

// Override Play! defaults to enable parallel test execution
Test / testOptions := Seq(Tests.Argument(TestFrameworks.Specs2, "junitxml", "console"))

//sourceGenerators in Compile <+= buildInfo

// sbt-github-packages configuration
githubOwner := "BandwidthOnDemand"
githubRepository := "play-nsi-support"
resolvers += Resolver.githubPackages("BandwidthOnDemand")

// sbt-native-packager configuration
enablePlugins(JavaAppPackaging)

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
