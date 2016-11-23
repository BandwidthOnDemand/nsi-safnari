name := "nsi-safnari"

version := "2.1.0-SNAPSHOT"

val surfnetNexusBaseUrl = "https://atlas.dlp.surfnet.nl/nexus/content/repositories"

libraryDependencies ++= Seq(
  ws,
  jdbc,
  anorm,
  "nl.surfnet" %% "play-nsi-support" % "2.1.1-SNAPSHOT",
  "nl.surfnet" %% "play-nsi-support" % "2.1.1-SNAPSHOT" % "test" classifier "tests",
  "org.scala-stm" %% "scala-stm" % "0.7",
  "org.postgresql" % "postgresql" % "9.2-1003-jdbc4",
  "com.mandubian" %% "play-json-zipper" % "1.2",
  "org.specs2" %% "specs2-scalacheck" % "2.3.13" % "test",
  "com.typesafe.akka" %% "akka-testkit" % "2.3.4" % "test"
)

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.8"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xlint", "-Ywarn-unused", "-Ywarn-unused-import", "-Ywarn-value-discard", "-Ywarn-adapted-args")

val surfnetSnapshots = "SURFnet Snapshots" at s"$surfnetNexusBaseUrl/snapshots"
val surfnetReleases = "SURFnet Releases" at s"$surfnetNexusBaseUrl/releases"

resolvers ++= Seq(
  surfnetSnapshots, surfnetReleases,
  "mandubian maven bintray (play-json-zipper)" at "http://dl.bintray.com/mandubian/maven",
  "SURFnet thirdparty" at s"$surfnetNexusBaseUrl/thirdparty",
  "SURFnet BoD Snapshots" at s"$surfnetNexusBaseUrl/public-snapshots",
  "SURFnet BoD Releases" at s"$surfnetNexusBaseUrl/public-releases"
)

javaOptions in Test += "-Dconfig.file=conf/test.conf"

testFrameworks in Test := Seq(TestFrameworks.Specs2)

// Override Play! defaults to enable parallel test execution
testOptions in Test := Seq(Tests.Argument(TestFrameworks.Specs2, "junitxml", "console"))

buildInfoSettings

sourceGenerators in Compile <+= buildInfo

val gitHeadCommitSha = settingKey[String]("git HEAD SHA")

gitHeadCommitSha := Process("git rev-parse --short HEAD").lines.head

buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, gitHeadCommitSha)

buildInfoPackage := "nl.surfnet.safnari"

PublishDist.publishSettings

publishTo := { if (isSnapshot.value) Some(surfnetSnapshots) else Some(surfnetReleases) }

// Disable ScalaDoc generation
sources in (Compile, doc) := Seq.empty
publishArtifact in (Compile, packageDoc) := false

sources in (Test, doc) := Seq.empty
publishArtifact in (Test, packageDoc) := false

net.virtualvoid.sbt.graph.Plugin.graphSettings

lazy val licenseText = settingKey[String]("Project license text.")

licenseText := IO.read(baseDirectory.value / "LICENSE")

headers := Map(
  "scala" -> (
    HeaderPattern.cStyleBlockComment,
    licenseText.value.split("\n").map {
      case ""   => " *"
      case line => " * " ++ line
    }.mkString("/*\n", "\n", "\n */\n")
  )
)
