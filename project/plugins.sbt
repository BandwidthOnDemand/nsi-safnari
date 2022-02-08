// Comment to get more information during initialization
logLevel := Level.Warn

// resolvers += Resolver.typesafeRepo("releases")
resolvers += "typesafe ivy-releases" at "https://repo.typesafe.com/typesafe/ivy-releases/"
resolvers += "typesafe maven-releases" at "https://repo.typesafe.com/typesafe/maven-releases/"

resolvers += "Maven Central Server" at "https://repo1.maven.org/maven2"
resolvers += "Typesafe Server" at "https://repo.typesafe.com/typesafe/releases"
//resolvers += "mandubian maven bintray" at "http://dl.bintray.com/mandubian/maven"
//resolvers += "Java.net Maven2 Repository" at "http://download.java.net/maven/2/"

addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.7.9")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.10.0")
addSbtPlugin("com.typesafe.sbt" % "sbt-less" % "1.1.2")
addSbtPlugin("com.typesafe.sbt" % "sbt-twirl" % "1.5.1")
addSbtPlugin("com.codecommit" % "sbt-github-packages" % "0.5.3")
addSbtPlugin("com.github.sbt" % "sbt-native-packager" % "1.9.7")
addSbtPlugin("com.github.sbt" % "sbt-release" % "1.1.0")
