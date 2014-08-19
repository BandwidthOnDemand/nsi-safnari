import sbt._

object SafnariBuild extends Build {
  lazy val mavenCommand = SettingKey[String]("maven-command", "Command to run maven")
  lazy val deployDist = taskKey[File]("Deploy distribution using maven")
}

