// Comment to get more information during initialization
logLevel := Level.Warn

addSbtPlugin("org.playframework" % "sbt-plugin" % "3.0.12")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.13.2")
addSbtPlugin("com.typesafe.sbt" % "sbt-less" % "1.1.2")
addSbtPlugin("org.playframework.twirl" % "sbt-twirl" % "2.0.9")
addSbtPlugin("com.codecommit" % "sbt-github-packages" % "0.5.3")
addSbtPlugin("com.github.sbt" % "sbt-native-packager" % "1.13.0")
addSbtPlugin("com.github.sbt" % "sbt-dynver" % "5.1.1")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.6.2")
