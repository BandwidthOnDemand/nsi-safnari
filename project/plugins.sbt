// Comment to get more information during initialization
logLevel := Level.Warn

// resolvers += Resolver.typesafeRepo("releases")
resolvers += "typesafe ivy-releases" at "https://repo.typesafe.com/typesafe/ivy-releases/"
resolvers += "typesafe maven-releases" at "https://repo.typesafe.com/typesafe/maven-releases/"

resolvers += "Maven Central Server" at "https://repo1.maven.org/maven2"
resolvers += "Typesafe Server" at "https://repo.typesafe.com/typesafe/releases"
//resolvers += "mandubian maven bintray" at "http://dl.bintray.com/mandubian/maven"
//resolvers += "Java.net Maven2 Repository" at "http://download.java.net/maven/2/"

addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.6.25")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.3.1")

//addSbtPlugin("com.typesafe.sbt" % "sbt-twirl" % "1.5.1")
