package com.github.windymelt.zmm

import com.monovore.decline._
import com.monovore.decline.effect._

sealed trait ZmmOption
final case class ShowCommand(target: String) extends ZmmOption // 今のところvoicevoxしか入らない
final case class TargetFile(target: java.nio.file.Path) extends ZmmOption
final case class InitializeCommand() extends ZmmOption

object CliOptions {
  private val showCommand = Opts.subcommand(name = "show", help = "Prints information.")(Opts.argument[String]("voicevox").map(ShowCommand.apply))
  private val targetFile = Opts.argument[java.nio.file.Path](metavar = "XMLFile").map(TargetFile.apply)
  private val initCommand = Opts.subcommand(name = "init", help = "Initializes current directory as ZMM project.")(Opts.unit.map(_ => InitializeCommand()))
  val opts: Opts[ZmmOption] = targetFile orElse showCommand orElse initCommand
}
