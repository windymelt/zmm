package com.github.windymelt.zmm

import cats.effect.IO
import cats.effect.IOApp
import cats.effect.ExitCode
import java.io.OutputStream
import org.http4s.syntax.header

import com.monovore.decline._
import com.monovore.decline.effect._

sealed trait ZmmOption
final case class ShowCommand(target: String) extends ZmmOption // 今のところvoicevoxしか入らない
final case class TargetFile(target: java.nio.file.Path) extends ZmmOption

object Main extends CommandIOApp(
  name = "zmm",
  header = "Zunda Movie Maker",
  version = BuildInfo.version,
) {
  val showCommand = Opts.subcommand(name = "show", help = "Prints information.")(Opts.argument[String]("voicevox").map(ShowCommand.apply))
  val targetFile = Opts.argument[java.nio.file.Path](metavar = "XMLFile").map(TargetFile.apply)
  val opts: Opts[ZmmOption] = targetFile orElse showCommand
  override def main: Opts[IO[ExitCode]] = opts map { o =>
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
