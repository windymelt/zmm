package com.github.windymelt.zmm

import cats.effect.IO
import cats.effect.IOApp
import cats.effect.ExitCode
import java.io.OutputStream
import org.http4s.syntax.header
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp

object Main extends CommandIOApp(
  name = "zmm",
  header = "Zunda Movie Maker",
  version = BuildInfo.version,
) {
  override def main: Opts[IO[ExitCode]] = CliOptions.opts map { o =>
    val cli = new Cli()
    o match {
      case ShowCommand(target) => target match {
        case "voicevox" => cli.showVoiceVoxSpeakers() >> IO.pure(cats.effect.ExitCode.Success)
        case _ => IO.println("subcommand [show] only accepts 'voicevox'. try `show voicevox`") >> IO.pure(cats.effect.ExitCode.Error)
      }
      case TargetFile(file) =>
        cli.generate(file.toString) >>
          IO.pure(cats.effect.ExitCode.Success)
    }
  }
}
