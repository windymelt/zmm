package com.github.windymelt.zmm

import com.monovore.decline._
import com.monovore.decline.effect._
import cats.implicits._

sealed trait ZmmOption
final case class ShowCommand(target: String)
    extends ZmmOption // 今のところvoicevoxしか入らない
final case class Generate(
    targetFile: TargetFile,
    outputFile: java.nio.file.Path
) extends ZmmOption
final case class InitializeCommand() extends ZmmOption
final case class VersionFlag() extends ZmmOption

final case class TargetFile(target: java.nio.file.Path)

object CliOptions {
  private val showCommand =
    Opts.subcommand(name = "show", help = "Prints information.")(
      Opts.argument[String]("voicevox").map(ShowCommand.apply)
    )
  private val targetFile =
    Opts.argument[java.nio.file.Path](metavar = "XMLFile").map(TargetFile.apply)
  private val outputFile =
    Opts
      .option[java.nio.file.Path](
        "output",
        help = "Output file name",
        short = "o",
        metavar = "OUTPUT.mp4"
      )
      .withDefault(java.nio.file.Path.of("output_with_bgm.mp4"))
  private val generate = (targetFile, outputFile) mapN { (tgt, out) =>
    Generate(tgt, out)
  }
  private val initCommand = Opts.subcommand(
    name = "init",
    help = "Initializes current directory as ZMM project."
  )(Opts.unit.map(_ => InitializeCommand()))
  private val versionOption = Opts
    .flag("version", help = "Show version", short = "v")
    .map(_ => VersionFlag())
  val opts: Opts[ZmmOption] =
    versionOption orElse generate orElse showCommand orElse initCommand
}
