package com.github.windymelt.zmm

import cats.data.NonEmptyList
import cats.data.Validated
import cats.implicits._
import com.monovore.decline._
import com.monovore.decline.effect._

/** ZMMの動作モードを決定するオプション。これらのうちいずれかを選択してZMMは起動する
  */
sealed trait ZmmOption

/** 設定等を表示するためのモード。
  *
  * @param target
  */
final case class ShowCommand(target: String)
    extends ZmmOption // 今のところvoicevoxしか入らない
/** 動画生成を行うモード。
  *
  * @param targetFile
  *   原稿XMLファイルへのパス
  * @param outputFile
  *   出力先MP4ファイルへのパス
  * @param screenShotBackend
  *   スクリーンショットのバックエンドに何を使うか
  */
final case class Generate(
    targetFile: TargetFile,
    outputFile: java.nio.file.Path,
    screenShotBackend: Option[ScreenShotBackend]
) extends ZmmOption

/** ディレクトリをZMMのプロジェクトとして初期化するモード。
  */
final case class InitializeCommand() extends ZmmOption

/** バージョンを表示させる。
  */
final case class VersionFlag() extends ZmmOption

final case class TargetFile(target: java.nio.file.Path)

sealed trait ScreenShotBackend
object ScreenShotBackend {
  final case object Chrome extends ScreenShotBackend
  final case object Firefox extends ScreenShotBackend
}

object CliOptions {
  private val showCommand =
    Opts.subcommand(name = "show", help = "Prints information.")(
      Opts.argument[String]("voicevox").map(ShowCommand.apply)
    )

  private val targetFile =
    Opts
      .argument[java.nio.file.Path](metavar = "XMLFile")
      .map(TargetFile.apply)

  private val outputFile =
    Opts
      .option[java.nio.file.Path](
        "output",
        help = "Output file name",
        short = "o",
        metavar = "OUTPUT.mp4"
      )
      .withDefault(java.nio.file.Path.of("output_with_bgm.mp4"))

  private val screenShotBackend = Opts
    .option[String](
      "screenshot",
      help = "Backend for screenshot. chrome or firefox.",
      short = "s",
      metavar = "chrome | firefox"
    )
    .mapValidated {
      case "chrome" =>
        Validated.valid(ScreenShotBackend.Chrome)
      case "firefox" => Validated.valid(ScreenShotBackend.Firefox)
      case _ =>
        Validated.invalid(
          "screenshot backend should be one of chrome and firefox"
            .pure[NonEmptyList]
        )
    }
    .orNone

  private val generate =
    (targetFile, outputFile, screenShotBackend) mapN (Generate.apply)

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
