import Dependencies._
import ReleaseTransformations._

ThisBuild / scalaVersion     := "2.13.8"
ThisBuild / organization     := "com.github.windymelt"
ThisBuild / organizationName := "windymelt"

lazy val root = (project in file("."))
  .settings(
    name := "zmm",
    publish / skip := true,
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-xml" % "1.2.0",
      "org.typelevel" %% "cats-effect" % "3.3.12",
      "org.http4s" %% "http4s-ember-client" % "0.23.16",
      "org.http4s" %% "http4s-circe" % "0.23.16",
      "io.circe" %% "circe-generic" % "0.14.3",
      "io.circe" %% "circe-literal" % "0.14.3",
      "com.lihaoyi" %% "os-lib" % "0.8.0",
      "com.typesafe" % "config" % "1.4.2",
      "com.monovore" %% "decline" % "2.4.1",
      "com.monovore" %% "decline-effect" % "2.4.1",
      "com.mitchtalmadge" % "ascii-data" % "1.4.0",
      "org.slf4j" % "slf4j-simple" % "2.0.6",
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
    buildInfoPackage := "com.github.windymelt.zmm"
  )
  .settings(
    releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies,              // : ReleaseStep
      inquireVersions,                        // : ReleaseStep
      runClean,                               // : ReleaseStep
      runTest,                                // : ReleaseStep
      setReleaseVersion,                      // : ReleaseStep
      commitReleaseVersion,                   // : ReleaseStep, performs the initial git checks
      tagRelease,                             // : ReleaseStep
      // publishArtifacts, // : ReleaseStep, checks whether `publishTo` is properly set up
      releaseStepTask(assembly),
      releaseStepTask(docker / publish)
      setNextVersion,                         // : ReleaseStep
      commitNextVersion,                      // : ReleaseStep
      pushChanges                             // : ReleaseStep, also checks that an upstream branch is properly configured
    )
  )
  .settings(
    dockerBaseImage := "amazoncorretto:17",
    Docker / daemonUser := "root",
    Docker / maintainer := "Windymelt",
    dockerRepository := Some("docker.io"),
    dockerUsername := Some("windymelt"),
    dockerUpdateLatest := true,
  )

ThisBuild / assemblyMergeStrategy := {
  case PathList("META-INF", "versions", "9", "module-info.class") => MergeStrategy.first
  case x =>
    val oldStrategy = (ThisBuild / assemblyMergeStrategy).value
    oldStrategy(x)
}




// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
