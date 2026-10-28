import scala.sys.process.Process

organization := "nl.surfnet"
name := "nsi-safnari"

Global / onChangedBuildSource := ReloadOnSourceChanges

githubTokenSource := (
  TokenSource.GitConfig("github.token")
    || TokenSource.Environment("GITHUB_USERTOKEN")
    || TokenSource.Environment("GITHUB_TOKEN")
)

scalaVersion := "3.3.8"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Wunused:imports,privates,locals,params",
  "-release:21"
)

ThisBuild / dynverVTagPrefix := false

lazy val mavenCommand = SettingKey[String]("maven-command", "Command to run maven")
lazy val deployDist = taskKey[File]("Deploy distribution using maven")

val playVersion = "3.0.12"
val playNsiSupportVersion = "3.1.1"

// play-test's unused WithBrowser helper pulls Appium through FluentLenium.
ThisBuild / excludeDependencies += ExclusionRule("io.appium")

libraryDependencies ++= Seq(
  guice,
  ws,
  jdbc,
  evolutions,
  "org.scala-stm" %% "scala-stm" % "0.11.1",
  "org.postgresql" % "postgresql" % "42.7.14",
  "org.specs2" %% "specs2-junit" % "4.23.0" % "test",
  "org.specs2" %% "specs2-matcher-extra" % "4.23.0" % "test",
  "org.specs2" %% "specs2-scalacheck" % "4.23.0" % "test",
  "org.apache.pekko" %% "pekko-testkit" % "1.0.3" % "test",
  "org.playframework" %% "play-test" % playVersion % "test",
  "org.playframework" %% "play-specs2" % playVersion % "test",
  "org.glassfish.hk2" % "osgi-resource-locator" % "3.0.0" % "test",
  "com.sun.xml.ws" % "jaxws-rt" % "4.0.5" % "test",
  // plexus-utils 4 moved its xml classes here; maven-model under play-test still needs them.
  "org.codehaus.plexus" % "plexus-xml" % "3.1.0" % "test",
  "nl.surfnet" %% "play-nsi-support" % playNsiSupportVersion,
  "nl.surfnet" %% "play-nsi-support" % playNsiSupportVersion % "test" classifier "tests"
)

dependencyOverrides ++= Seq(
  "com.fasterxml.jackson.core" % "jackson-core" % "2.22.4",
  "com.fasterxml.jackson.core" % "jackson-databind" % "2.22.4",
  "com.fasterxml.jackson.core" % "jackson-annotations" % "2.22",
  "com.fasterxml.jackson.datatype" % "jackson-datatype-jdk8" % "2.22.4",
  "com.fasterxml.jackson.datatype" % "jackson-datatype-jsr310" % "2.22.4",
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.22.4",
  "commons-beanutils" % "commons-beanutils" % "1.11.0",
  "org.apache.commons" % "commons-lang3" % "3.21.0",
  "org.codehaus.plexus" % "plexus-utils" % "4.1.0",
  "ch.qos.logback" % "logback-core" % "1.6.5",
  "ch.qos.logback" % "logback-classic" % "1.6.5",
  "at.yawk.lz4" % "lz4-java" % "1.12.0",
  "org.jsoup" % "jsoup" % "1.23.2",
  // Scala 3 compiler/scaladoc toolchain only (scalaInstance), never on the app classpath.
  "org.jline" % "jline-reader" % "3.30.17",
  "org.jline" % "jline-terminal" % "3.30.17",
  "org.jline" % "jline-terminal-jni" % "3.30.17",
  "org.jline" % "jline-native" % "3.30.17",
  // Selenium/HtmlUnit test-only deps (via play-test's WithBrowser helper, unused by this
  // app): opentelemetry-api and jetty-http can move within their own line without
  // disturbing the selenium-api version htmlunit-driver 4.13.0 was built against.
  "io.opentelemetry" % "opentelemetry-api" % "1.67.0",
  "org.eclipse.jetty" % "jetty-http" % "9.4.58.v20250814",
  "org.eclipse.jetty" % "jetty-client" % "9.4.58.v20250814",
  "org.eclipse.jetty" % "jetty-io" % "9.4.58.v20250814",
  "org.eclipse.jetty" % "jetty-util" % "9.4.58.v20250814",
  "org.eclipse.jetty.websocket" % "websocket-api" % "9.4.58.v20250814",
  "org.eclipse.jetty.websocket" % "websocket-client" % "9.4.58.v20250814",
  "org.eclipse.jetty.websocket" % "websocket-common" % "9.4.58.v20250814"
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

// Compile all theme entry points (variables-*.less) instead of the default main.less
Assets / LessKeys.less / includeFilter := "theme-*.less"

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
