package com.github.windymelt.zmm

import cats.effect.IO
import cats.effect.IOApp
import cats.effect.ExitCode
import java.io.OutputStream
import org.http4s.syntax.header

object Main extends IOApp {
  def run(args: List[String]) = {
    // load source file
    val filePath = args(0)

    val cli = new Cli()
    cli.generate(filePath) *>
      IO.pure(cats.effect.ExitCode.Success)
  }
}
