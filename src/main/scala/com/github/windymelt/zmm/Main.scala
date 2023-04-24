package com.github.windymelt.zmm

import cats.effect.IO
import cats.effect.IOApp
import cats.effect.ExitCode
import java.io.OutputStream
import org.http4s.syntax.header
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp

object Main
    extends CommandIOApp(
      name = "zmm",
      header =
        "Zunda Movie Maker -- see https://www.3qe.us/zmm/doc/ for more documentation"
    ) {
  override def main: Opts[IO[ExitCode]] = CliOptions.opts map { o =>
    val defaultCli = new ChromiumCli()

    o match {
      case VersionFlag() => defaultCli.showVersion >> IO.pure(ExitCode.Success)
      case ShowCommand(target) =>
        target match {
          case "voicevox" =>
            defaultCli.showVoiceVoxSpeakers() >> IO.pure(ExitCode.Success)
          case _ =>
            IO.println(
              "subcommand [show] only accepts 'voicevox'. try `show voicevox`"
            ) >> IO.pure(ExitCode.Error)
        }
      case TargetFile(file, screenShotBackend) =>
        val cli = screenShotBackend match {
          // TODO: ffmpeg verbosityをcli opsから設定可能にする
          case Some(ScreenShotBackend.Chrome)  => new ChromiumCli()
          case Some(ScreenShotBackend.Firefox) => new FirefoxCli()
          case _                               => defaultCli
        }
        cli.generate(file.toString) >>
          IO.pure(ExitCode.Success)
      case InitializeCommand() =>
        defaultCli.initializeProject() >> IO.pure(ExitCode.Success)
    }
  }
}
