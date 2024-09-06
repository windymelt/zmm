import Dependencies._
import com.typesafe.sbt.packager.docker._

import ReleaseTransformations._

ThisBuild / scalaVersion := "3.4.1"
ThisBuild / organization := "com.github.windymelt"
ThisBuild / organizationName := "windymelt"

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val root = (project in file("."))
  .settings(
    name := "zmm",
    publish / skip := true,
    scalacOptions ++= Seq("-deprecation", "-Wunused:all"),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.5.4",
      "org.http4s" %% "http4s-ember-client" % "0.23.26",
      "org.http4s" %% "http4s-circe" % "0.23.26",
      "io.circe" %% "circe-generic" % "0.14.10",
      "io.circe" %% "circe-parser" % "0.14.10",
      "io.circe" %% "circe-literal" % "0.14.10",
      "io.circe" %% "circe-optics" % "0.15.0",
      "com.lihaoyi" %% "os-lib" % "0.9.3",
      "com.typesafe" % "config" % "1.4.3",
      "com.monovore" %% "decline" % "2.4.1",
      "com.monovore" %% "decline-effect" % "2.4.1",
      "com.mitchtalmadge" % "ascii-data" % "1.4.0",
      "ch.qos.logback" % "logback-classic" % "1.4.7",
      "org.typelevel" %% "log4cats-slf4j" % "2.6.0",
      "org.wvlet.airframe" %% "airframe" % "24.4.0",
      scalaTest % Test,
    ),
    assembly / mainClass := Some("com.github.windymelt.zmm.Main"),
  )
  .enablePlugins(SbtTwirl)
  .enablePlugins(BuildInfoPlugin)
  .enablePlugins(JavaAppPackaging) // for DockerPlugin
  .enablePlugins(DockerPlugin)
  .settings(
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "com.github.windymelt.zmm",
  )
  .settings(
    releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies, // : ReleaseStep
      inquireVersions, // : ReleaseStep
      runClean, // : ReleaseStep
      runTest, // : ReleaseStep
      setReleaseVersion, // : ReleaseStep
      commitReleaseVersion, // : ReleaseStep, performs the initial git checks
      tagRelease, // : ReleaseStep
      // publishArtifacts, // : ReleaseStep, checks whether `publishTo` is properly set up
      releaseStepTask(assembly),
      releaseStepTask(Docker / publish),
      setNextVersion, // : ReleaseStep
      commitNextVersion, // : ReleaseStep
      pushChanges, // : ReleaseStep, also checks that an upstream branch is properly configured
    ),
  )
  .settings(
    dockerBaseImage := "amazoncorretto:17",
    Docker / daemonUser := "root",
    Docker / maintainer := "Windymelt",
    dockerRepository := Some("docker.io"),
    dockerUsername := Some("windymelt"),
    dockerUpdateLatest := true,
    Universal / mappings += file("entrypoint.sh") -> "entrypoint.sh",
    /* zmmではScala highlightのためにカスタムしたhighlight.jsを同梱しているが、mappingが今のところ壊れているのでDocker Imageでは直接highlight.jsをダウンロードさせる */
    dockerCommands ++= Seq(
      // Initnally, run as root. Go to protected user inside entrypoint.sh.
      Cmd("USER", "root"),
      // coretto image does not have useradd utils
      ExecCmd("RUN", "yum", "install", "-y", "shadow-utils", "unzip"),
      ExecCmd("RUN", "yum", "clean", "all"),
      // Add protected user. entrypoint.sh uses this.
      ExecCmd("RUN", "useradd", "-m", "zundamon"),
      ExecCmd("RUN", "mkdir", "/app"),
      ExecCmd("RUN", "mkdir", "-p", "/app/artifacts/html"),
      ExecCmd("RUN", "mkdir", "/app/assets"),
      // Install dependencies
      // font
      ExecCmd(
        "ADD",
        "https://ftp.iij.ad.jp/pub/osdn.jp/users/8/8574/rounded-mplus-20150529.zip",
        "/tmp/mplus.zip",
      ),
      ExecCmd(
        "RUN",
        "unzip",
        "-d",
        "/usr/share/fonts",
        "/tmp/mplus.zip",
      ),
      ExecCmd(
        "RUN",
        "rm",
        "/tmp/mplus.zip",
      ),
      ExecCmd(
        "RUN",
        "fc-cache",
      ),
      ExecCmd(
        "ADD",
        "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/highlight.min.js",
        "/app/highlight.min.js",
      ),
      ExecCmd("RUN", "mkdir", "-p", "/app/highlight/styles"),
      ExecCmd(
        "ADD",
        "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/styles/default.min.css",
        "/app/highlight/styles/default.min.css",
      ),
      Cmd("WORKDIR", "/root"),
      ExecCmd("RUN", "yum", "-y", "install", "wget", "tar", "xz"),
      ExecCmd(
        "RUN",
        "wget",
        "https://johnvansickle.com/ffmpeg/releases/ffmpeg-release-amd64-static.tar.xz",
      ),
      ExecCmd("RUN", "tar", "xvf", "ffmpeg-release-amd64-static.tar.xz"),
      // output directory sometimes changes according to latest version.
      ExecCmd("RUN", "mv", "ffmpeg-6.1-amd64-static/ffmpeg", "/usr/bin/ffmpeg"),
      ExecCmd(
        "RUN",
        "mv",
        "ffmpeg-6.1-amd64-static/ffprobe",
        "/usr/bin/ffprobe",
      ),
      ExecCmd(
        "RUN",
        "rm",
        "-rf",
        "ffmpeg-release-amd64-static.tar.xz",
        "ffmpeg-6.1-amd64-static/",
      ),
      ExecCmd("RUN", "amazon-linux-extras", "install", "-y", "epel"),
      ExecCmd("RUN", "yum", "update", "-y"),
      ExecCmd("RUN", "yum", "install", "-y", "chromium"),
      // entrypoint.sh is automatically copied into /opt/docker by sbt-native-packager.
      ExecCmd("RUN", "chmod", "u+x", "/opt/docker/entrypoint.sh"),
      ExecCmd("RUN", "chown", "-R", "zundamon", "/app"),
      Cmd("ENV", "IS_DOCKER_ZMM=1"),
      Cmd("WORKDIR", "/app"),
    ),
    dockerEntrypoint := Seq("/opt/docker/entrypoint.sh"),
  )

ThisBuild / assemblyMergeStrategy := {
  case PathList("META-INF", "versions", "9", "module-info.class") =>
    MergeStrategy.first
  case PathList("module-info.class") =>
    MergeStrategy.first
  case x =>
    val oldStrategy = (ThisBuild / assemblyMergeStrategy).value
    oldStrategy(x)
}

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
