// Comment to get more information during initialization
logLevel := Level.Warn

resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.3.3")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.3.1")

addSbtPlugin("com.typesafe.sbt" % "sbt-less" % "1.0.0")
