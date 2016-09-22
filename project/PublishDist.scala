import sbt._
import Keys._
import com.typesafe.sbt.SbtNativePackager._

object PublishDist {
  lazy val dist = com.typesafe.sbt.SbtNativePackager.NativePackagerKeys.dist

  val publishDist = TaskKey[sbt.File]("publish-dist", "publish the dist artifact")

  lazy val publishSettings = sbtrelease.ReleasePlugin.releaseSettings ++ Seq(
    publishArtifact in (Compile, packageDoc) := false,
    publishArtifact in (Compile, packageSrc) := false,
    publishArtifact in (Compile, packageBin) := false,

    publish <<= (publish) dependsOn dist,
    publishLocal <<= (publishLocal) dependsOn dist,

    artifact in publishDist ~= {
      (art: Artifact) => art.copy(`type` = "zip", extension = "zip")
    },

    // disable using the Scala version in output paths and artifacts
    crossPaths := false,
    publishMavenStyle := true,
    pomIncludeRepository := {
      x => false
    },

    publishDist <<= (target in Universal, normalizedName, version) map { (targetDir, id, version) =>
      val packageName = "%s-%s" format(id, version)
      targetDir / (packageName + ".zip")
    }

  ) ++ addArtifact(artifact in publishDist, publishDist)
}
