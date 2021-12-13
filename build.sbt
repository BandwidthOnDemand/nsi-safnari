organization := "nl.surfnet"
name := "nsi-safnari"

//releaseSettings


libraryDependencies ++= Seq(
  ws,
  jdbc,
  evolutions,
  "org.scala-stm" %% "scala-stm" % "0.7",
  "org.postgresql" % "postgresql" % "42.3.1",
  "com.github.michaelahlers" % "play-json-zipper_2.11" % "1.2.0.23.1",
  "org.specs2" %% "specs2-scalacheck" % "3.6.6" % "test",
  "org.specs2" %% "specs2-junit" % "3.6.6" % "test",
  "org.specs2" %% "specs2-matcher-extra" % "3.6.6" % "test",
  "com.typesafe.akka" %% "akka-testkit" % "2.3.13" % "test"
)

lazy val root = (project in file(".")).enablePlugins(PlayScala)
    .dependsOn(sub1 % "compile->compile;test->test")
lazy val sub1 = RootProject( uri("git://github.com/BandwidthOnDemand/play-nsi-support.git") )

scalaVersion := "2.11.12"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xlint", "-Ywarn-unused", "-Ywarn-value-discard", "-target:jvm-1.8")

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

//PublishDist.publishSettings

// Disable ScalaDoc generation
sources in (Compile, doc) := Seq.empty
publishArtifact in (Compile, packageDoc) := false

sources in (Test, doc) := Seq.empty
publishArtifact in (Test, packageDoc) := false

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
