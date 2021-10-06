// Comment to get more information during initialization
logLevel := Level.Warn

// resolvers += Resolver.typesafeRepo("releases")
resolvers += "typesafe ivy-releases" at "https://repo.typesafe.com/typesafe/ivy-releases/"
resolvers += "typesafe maven-releases" at "https://repo.typesafe.com/typesafe/maven-releases/"

resolvers += "Maven Central Server" at "https://repo1.maven.org/maven2"
resolvers += "Typesafe Server" at "https://repo.typesafe.com/typesafe/releases"
resolvers += "mandubian maven bintray" at "http://dl.bintray.com/mandubian/maven"
resolvers += "Java.net Maven2 Repository" at "http://download.java.net/maven/2/"

addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.3.10")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.3.1")

addSbtPlugin("com.typesafe.sbt" % "sbt-less" % "1.0.0")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "0.8.5")

addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.7.4")

addSbtPlugin("de.heikoseeberger" % "sbt-header" % "1.1.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "0.7.4")

// addSbtPlugin("com.mandubian" % "play-json-zipper" % "1.2")

///addSbtPlugin("com.typesafe.play" % "play-json" % "2.3.4")

// scalaVersion := "2.10.2"

// libraryDependencies += "play" % "play_2.10" % "2.1.0"

// addMavenResolverPlugin
