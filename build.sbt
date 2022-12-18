import Dependencies._

ThisBuild / scalaVersion     := "2.13.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.github.windymelt"
ThisBuild / organizationName := "windymelt"

lazy val root = (project in file("."))
  .settings(
    name := "zmm",
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
      scalaTest % Test,
    )
  )
  .enablePlugins(SbtTwirl)
  .enablePlugins(BuildInfoPlugin)
  .settings(
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "com.github.windymelt.zmm"
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
