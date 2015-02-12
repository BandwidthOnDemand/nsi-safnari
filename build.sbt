name := "nsi-safnari"

version := "1.0-SNAPSHOT"

val nexusBaseUri = "https://atlas.dlp.surfnet.nl/nexus/content/repositories"

libraryDependencies ++= Seq(
  ws,
  jdbc,
  anorm,
  "nl.surfnet" %% "play-nsi-support" % "1.3-SNAPSHOT",
  "nl.surfnet" %% "play-nsi-support" % "1.3-SNAPSHOT" % "test" classifier "tests",
  "org.scala-stm" %% "scala-stm" % "0.7",
  "org.postgresql" % "postgresql" % "9.2-1003-jdbc4",
  "com.mandubian" %% "play-json-zipper" % "1.2",
  "org.specs2" %% "specs2-scalacheck" % "2.3.13" % "test",
  "com.typesafe.akka" %% "akka-testkit" % "2.3.4" % "test"
)

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.4"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xlint", "-Ywarn-unused", "-Ywarn-unused-import", "-Ywarn-value-discard", "-Ywarn-adapted-args")

resolvers ++= Seq(
    "mandubian maven bintray (play-json-zipper)" at "http://dl.bintray.com/mandubian/maven",
    "SURFnet thirdparty" at s"$nexusBaseUri/thirdparty",
    "SURFnet BoD Snapshots" at s"$nexusBaseUri/public-snapshots",
    "SURFnet BoD Releases" at s"$nexusBaseUri/public-releases"
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

mavenCommand := "mvn"

deployDist := {
  val distFile = com.typesafe.sbt.packager.Keys.dist.value
  val version = Keys.version.value
  val (repoId, repoUrl) = if (version.trim.endsWith("-SNAPSHOT")) ("surfnet-snapshots", s"$nexusBaseUri/snapshots") else ("surfnet-releases", s"$nexusBaseUri/releases")
  val groupId = organization.value
  val artifactId = artifact.value.name
  val maven = mavenCommand.value
  val command = Seq(
    maven, "-B", "deploy:deploy-file",
    s"-DrepositoryId=$repoId", s"-Durl=$repoUrl", s"-Dfile=$distFile",
    s"-DgroupId=$groupId", s"-DartifactId=$artifactId", s"-Dversion=$version", "-Dpackaging=zip"
  )
  println(s"Deploying $distFile to $repoId at $repoUrl using command:\n ${command.mkString(" ")}")
  Process(command).lines.foreach(println)
  distFile
}

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
