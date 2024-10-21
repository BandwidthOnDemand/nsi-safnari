import scala.sys.process.Process

organization := "nl.surfnet"
name := "nsi-safnari"

Global / onChangedBuildSource := ReloadOnSourceChanges

githubTokenSource := (
  TokenSource.GitConfig("github.token")
    || TokenSource.Environment("GITHUB_USERTOKEN")
    || TokenSource.Environment("GITHUB_TOKEN")
)

scalaVersion := "3.3.4"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Wunused:imports,privates,locals,params",
  "-release:21"
)

//releaseSettings

lazy val mavenCommand = SettingKey[String]("maven-command", "Command to run maven")
lazy val deployDist = taskKey[File]("Deploy distribution using maven")

val playVersion = "3.0.5"
val playNsiSupportVersion = "3.0.0"

libraryDependencies ++= Seq(
  guice,
  ws,
  jdbc,
  evolutions,
  "org.scala-stm" %% "scala-stm" % "0.11.1",
  "org.postgresql" % "postgresql" % "42.5.0",
  "org.specs2" %% "specs2-junit" % "4.20.7" % "test",
  "org.specs2" %% "specs2-matcher-extra" % "4.20.7" % "test",
  "org.specs2" %% "specs2-scalacheck" % "4.20.7" % "test",
  "org.apache.pekko" %% "pekko-testkit" % "1.0.3" % "test",
  "org.playframework" %% "play-test" % playVersion % "test",
  "org.playframework" %% "play-specs2" % playVersion % "test",
  "org.glassfish.hk2" % "osgi-resource-locator" % "2.4.0" % "test",
  "com.sun.xml.ws" % "jaxws-rt" % "4.0.3" % "test",
  "nl.surfnet" %% "play-nsi-support" % playNsiSupportVersion,
  "nl.surfnet" %% "play-nsi-support" % playNsiSupportVersion % "test" classifier "tests"
)

val gitHeadCommitSha = settingKey[String]("git HEAD SHA")

gitHeadCommitSha := Process("git rev-parse --short HEAD").lineStream.head

lazy val root = (project in file("."))
  .enablePlugins(PlayScala)
  .enablePlugins(BuildInfoPlugin)
  .enablePlugins(SbtTwirl)
  .settings(
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, gitHeadCommitSha),
    buildInfoPackage := "nl.surfnet.safnari",
    exportJars := true
  )

Test / javaOptions += "-Dconfig.file=conf/test.conf"

Test / testFrameworks := Seq(TestFrameworks.Specs2)

// Override Play! defaults to enable parallel test execution
Test / testOptions := Seq(Tests.Argument(TestFrameworks.Specs2, "junitxml", "console"))

//sourceGenerators in Compile <+= buildInfo

// sbt-github-packages configuration
githubOwner := "BandwidthOnDemand"
githubRepository := "nsi-safnari"
resolvers += Resolver.githubPackages("BandwidthOnDemand")

// sbt-native-packager configuration
enablePlugins(JavaAppPackaging, UniversalDeployPlugin)

//PublishDist.publishSettings

// Disable ScalaDoc generation
Compile / doc / sources := Seq.empty
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
