package com.github.windymelt.zmm

import cats.effect.IO
import cats.effect.IOApp
import cats.effect.ExitCode
import java.io.OutputStream
import org.http4s.syntax.header

import com.monovore.decline._
import com.monovore.decline.effect._

object Main extends CommandIOApp(
  name = "zmm",
  header = "Zunda Movie Maker",
  version = BuildInfo.version,
) {
  val opts = Opts.argument[java.nio.file.Path](metavar = "XMLFile")
  override def main: Opts[IO[ExitCode]] = opts map { file =>
    val cli = new Cli()
    cli.generate(file.toString) *>
      IO.pure(cats.effect.ExitCode.Success)
  }
}
