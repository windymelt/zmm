package com.github.windymelt.zmm

import cats.effect.IO
import cats.effect.IOApp
import cats.effect.ExitCode
import java.io.OutputStream
import org.http4s.syntax.header
import com.github.windymelt.zmm.domain.model.Context
import scala.concurrent.duration.FiniteDuration

final class Cli
    extends domain.repository.FFmpegComponent
    with domain.repository.VoiceVoxComponent
    with domain.repository.ScreenShotComponent
    with infrastructure.FFmpegComponent
    with infrastructure.VoiceVoxComponent
    with infrastructure.ChromeScreenShotComponent
    with util.UtilComponent {

  val zmmLogo = """ _________  ______  ___
|___  /|  \/  ||  \/  |
   / / | .  . || .  . |
  / /  | |\/| || |\/| |
./ /___| |  | || |  | |
\_____/\_|  |_/\_|  |_/"""

  val voiceVoxUri =
    sys.env.get("VOICEVOX_URI") getOrElse config.getString("voicevox.apiUri")
  val chromiumCommand =
    sys.env.get("CHROMIUM_CMD").getOrElse(config.getString("chromium.command"))
  def voiceVox: VoiceVox = new ConcreteVoiceVox(voiceVoxUri)
  def ffmpeg =
    new ConcreteFFmpeg(config.getString("ffmpeg.command"), ConcreteFFmpeg.Quiet)
  val chromiumNoSandBox = sys.env
    .get("CHROMIUM_NOSANDBOX")
    .map(_ == "1")
    .getOrElse(config.getBoolean("chromium.nosandbox"))
  def screenShot = new ChromeScreenShot(
    chromiumCommand,
    ChromeScreenShot.Quiet,
    chromiumNoSandBox
  )

  def showVoiceVoxSpeakers(): IO[Unit] = {
    import io.circe.JsonObject
    import com.mitchtalmadge.asciidata.table.ASCIITable
    for {
      speakers <- voiceVox.speakers()
      speakersTable <- IO.pure {
        val speakersArray = speakers.asArray.get.flatMap(_.asObject)
        val styleToSeq = (name: String) =>
          (id: String) => (styleName: String) => Seq(name, id, styleName)
        val speakerToSeq = (speaker: JsonObject) => {
          val styles = speaker("styles").get.asArray.get.flatMap(_.asObject)
          styles map (s =>
            styleToSeq(speaker("name").get.asString.get)(
              s("id").get.asNumber.get.toString
            )(s("name").get.asString.get)
          )
        }
        speakersArray.flatMap(speakerToSeq).map(_.toArray).toArray
      }
      _ <- IO.println(
        ASCIITable.fromData(
          Seq("voice", "voice ID", "style").toArray,
          speakersTable
        )
      )
    } yield ()
  }

  /** 現在のディレクトリをZMMプロジェクトとして初期化する。
    *
    * 現在のディレクトリに原稿XMLファイルや生成物配置用のディレクトリを作成する。 既にディレクトリやファイルが存在している場合は何もしない。
    *
    * @return
    *   Unitを返す。
    */
  def initializeProject(): IO[Unit] = {
    val agreed = for {
      cwd <- IO.pure(os.pwd.toString())
      _ <- IO.print(s"$cwd を ZMMプロジェクトとして初期化しますか? [y/N]?>")
      ynString <- IO.readLine
    } yield ynString == "y"

    val placeXml: IO[Unit] = IO {
      os.exists(os.pwd / "script.xml") match {
        case true  => IO.println("script.xml は既に存在するのでスキップされました")
        case false => IO(os.write(os.pwd / "script.xml", xml.script().body))
      }
    }.flatten

    val digArtifacts: IO[Unit] = IO {
      os.exists(os.pwd / "artifacts") match {
        case true  => IO.println("artifacts/ は既に存在するのでスキップされました")
        case false => IO(os.makeDir(os.pwd / "artifacts"))
      }
    }.flatten

    val digArtifactsHtml: IO[Unit] = IO {
      os.exists(os.pwd / "artifacts" / "html") match {
        case true  => IO.println("artifacts/html/ は既に存在するのでスキップされました")
        case false => IO(os.makeDir(os.pwd / "artifacts" / "html"))
      }
    }.flatten

    val digAssets: IO[Unit] = IO {
      os.exists(os.pwd / "assets") match {
        case true  => IO.println("assets/ は既に存在するのでスキップされました")
        case false => IO(os.makeDir(os.pwd / "assets"))
      }
    }.flatten

    val init = for {
      _ <- placeXml
      _ <- digArtifacts >> digArtifactsHtml
      _ <- digAssets
    } yield ()

    // ZMMプロジェクトを構成するいくつかのファイル/ディレクトリについて、存在しなかったらテンプレートをもとに作成する、を繰り返す
    agreed flatMap {
      case true  => init
      case false => IO.println("中断します")
    }
  }

  def generate(filePath: String): IO[Unit] = {
    val content = IO.delay(scala.xml.XML.loadFile(filePath))

    for {
      _ <- showLogo
      _ <- IO.println(s"""[pwd] ${System.getProperty("user.dir")}""")
      _ <- IO.println(s"""[configuration] voicevox api: ${voiceVoxUri}""")
      _ <- IO.println(
        s"""[configuration] chromium command: ${chromiumCommand}"""
      )
      _ <- IO.println(s"""[configuration] ffmpeg command: ${config.getString(
          "ffmpeg.command"
        )}""")
      _ <- IO.println("Invoking audio api...")
      x <- content
      _ <- contentSanityCheck(x)
      defaultCtx <- prepareDefaultContext(x)
      _ <- applyDictionary(defaultCtx)
      //        _ <- IO.println(ctx)
      sayCtxPairs <- IO.pure(
        Context.fromNode((x \ "dialogue").head, defaultCtx)
      )
      pathAndDurations <- {
        import cats.syntax.parallel._
        val saySeq = sayCtxPairs map { case (s, ctx) =>
          generateSay(s, voiceVox, ctx)
        }
        saySeq.parSequence
      }
      // この時点でvideoとaudioとの間に依存がないので並列実行する
      // BUG: SI-5589 により、タプルにバインドできない
      va <- backgroundIndicator("Generating video and concatenated audio").use {
        _ =>
          generateVideo(sayCtxPairs, pathAndDurations) product ffmpeg
            .concatenateWavFiles(pathAndDurations.map(_._1.toString))
      }
      val (video, audio) = va
      zippedVideo <- backgroundIndicator("Zipping silent video and audio").use {
        _ => ffmpeg.zipVideoWithAudio(video, audio)
      }
      _ <- backgroundIndicator("Applying BGM").use { _ =>
        // BGMを合成する。BGMはコンテキストで割り当てる。sayCtxPairsでsayごとにコンテキストが確定するので、同じBGMであれば結合しつつ最終的なDurationを計算する。
        // たとえば、BGMa 5sec BGMa 5sec BGMb 10sec であるときは、 BGMa 10sec BGMb 10secに簡約される。
        val bgmWithDuration: Seq[(Option[os.Path], FiniteDuration)] =
          sayCtxPairs
            .map(p => p._2.bgm.map(os.pwd / os.RelPath(_)))
            .zip(pathAndDurations.map(_._2))

        val reductedBgmWithDuration = groupReduction(bgmWithDuration)

        // 環境によっては上書きに失敗する？ので出力ファイルが存在する場合削除する
        val outputFile = os.pwd / "output_with_bgm.mp4"
        os.remove(outputFile, checkExists = false)

        reductedBgmWithDuration.filter(_._1.isDefined).size match {
          case 0 =>
            IO.pure(
              os.move(zippedVideo, outputFile)
            ) // Dirty fix. TODO: fix here
          case _ =>
            ffmpeg.zipVideoWithAudioWithDuration(
              zippedVideo,
              reductedBgmWithDuration
            )
        }
      }
      _ <- IO.println("\nDone!")
    } yield ()
  }

  private def showLogo: IO[Unit] =
    IO.println(
      withColor(scala.io.AnsiColor.GREEN ++ scala.io.AnsiColor.BOLD)(zmmLogo)
    ) >>
      IO.println(withColor(scala.io.AnsiColor.GREEN)(s"${BuildInfo.version}"))

  /** ZMMのバージョンを表示する。
    *
    * デバッグや問い合わせの助けとしても使う。
    *
    * @return
    *   IO[Unit]
    */
  def showVersion: IO[Unit] =
    IO.print("zmm ver=") >>
      (BuildInfo.version match {
        case s"$_-SNAPSHOT" =>
          IO.print(withColor(scala.io.AnsiColor.YELLOW)(BuildInfo.version))
        case _ =>
          IO.print(withColor(scala.io.AnsiColor.GREEN)(BuildInfo.version))
      }) >>
      ((System.getenv("IS_DOCKER_ZMM") == "1") match {
        case true  => IO.print(withColor(scala.io.AnsiColor.CYAN)(" (Docker)"))
        case false => IO.unit
      }) >>
      IO.print(", scalaVer=") >>
      IO.print(withColor(scala.io.AnsiColor.GREEN)(BuildInfo.scalaVersion)) >>
      IO.print(", sbtVer=") >>
      IO.print(withColor(scala.io.AnsiColor.GREEN)(BuildInfo.sbtVersion)) >>
      IO.print(s", jvm=${System.getProperty("java.vm.name")}") >>
      IO.print(s", runtimeVer=${Runtime.version().toString()}") >>
      IO.print(s", vendor=${System.getProperty("java.vendor")}") >>
      IO.println("")

  /** 辞書要素を反映させる。
    *
    * 今のところVOICEVOX用の発音辞書に登録を行うだけだが、今後の開発によってはその他の音声合成ソフトウェアの辞書登録に使ってよい。
    *
    * @param ctx
    *   辞書を取り出す元となるコンテキスト
    * @return
    *   有用な情報は返されない
    */
  private def applyDictionary(ctx: Context): IO[Unit] = {
    import cats.syntax.parallel._
    val registerList = ctx.dict.map { d =>
      voiceVox.registerDict(d._1, d._2, d._3)
    }
    registerList.reduceLeft[IO[Unit]] { case (acc, i) => i >> acc }
  }

  private def generateSay(
      sayElem: domain.model.Say,
      voiceVox: VoiceVox,
      ctx: Context
  ): IO[(fs2.io.file.Path, scala.concurrent.duration.FiniteDuration)] = for {
    actualPronunciation <- IO.pure(
      ctx.sic.getOrElse(sayElem.text)
    ) // sicがない場合は元々のセリフを使う
    aq <- backgroundIndicator("Building Audio Query").use { _ =>
      // by属性がないことはないやろという想定でgetしている
      buildAudioQuery(
        actualPronunciation,
        ctx.spokenByCharacterId.get,
        voiceVox,
        ctx
      )
    }
//    _ <- IO.println(aq)
    fixedAq <- ctx.speed map (sp => voiceVox.controlSpeed(aq, sp)) getOrElse (IO
      .pure(aq))
    wav <- backgroundIndicator("Synthesizing wav").use { _ =>
      buildWavFile(fixedAq, ctx.spokenByCharacterId.get, voiceVox, ctx)
    }
    sha1Hex <- sha1HexCode(sayElem.text.getBytes())
    path <- backgroundIndicator("Exporting .wav file").use { _ =>
      writeStreamToFile(wav, s"artifacts/voice_${sha1Hex}.wav")
    }
    dur <- ffmpeg.getWavDuration(path.toString)
  } yield (path, dur)

  private def contentSanityCheck(elem: scala.xml.Elem): IO[Unit] = {
    val checkTopElem = elem.label == "content"
    val ver = elem \@ "version" == "0.0"

    if (!(checkTopElem && ver)) {
      throw new Exception("Invalid scenary XML") // TODO: 丁寧なエラーメッセージ
    }
    IO.unit
  }

  private def prepareDefaultContext(elem: scala.xml.Elem): IO[Context] = {
    val voiceConfigList = elem \ "meta" \ "voiceconfig"
    val voiceConfigMap = voiceConfigList.map { vc =>
      vc \@ "backend" match {
        case "voicevox" =>
          val vvc = vc \ "voicevoxconfig"
          val voiceVoxSpeakerId = vvc \@ "id"
          (vc \@ "id", domain.model.VoiceVoxBackendConfig(voiceVoxSpeakerId))
        case _ => ??? // not implemented
      }
    }.toMap

    val characterConfigList = elem \ "meta" \ "characterconfig"
    val characterConfigMap = characterConfigList.map { cc =>
      val name = cc \@ "name"
      val defaultSerifColor = Some(cc \@ "serif-color").filterNot(_.isEmpty())
      val tachieUrl = Some(cc \@ "tachie-url").filterNot(_.isEmpty())
      name -> domain.model.CharacterConfig(
        name,
        cc \@ "voice-id",
        defaultSerifColor,
        tachieUrl
      )
    }.toMap

    val defaultBackgroundImage =
      (elem \ "meta" \ "assets" \ "backgroundImage")
        .filter(_.attribute("id").map(_.text).contains("default"))
        .headOption
        .flatMap(_.attribute("url").headOption.map(_.text))

    val defaultFont = (elem \ "meta" \ "font").headOption.map(_.text)

    // 発音調整などに使う文字列辞書。今のところVOICEVOXの発音辞書に使っている
    // (word, pronounce, accent lower point)
    val dict: Seq[(String, String, Int)] =
      (elem \ "meta" \ "dict")
        .flatMap(es =>
          es.map(e =>
            (
              e.text,
              (e \@ "pronounce" filterNot (_ == '_')),
              (e \@ "pronounce" indexOf ('_'))
            )
          )
        )

    val codes: Map[String, (String, Option[String])] =
      (elem \ "predef" \ "code")
        .flatMap(es =>
          es.map { e =>
            val code = e.text.stripLeading()
            val id = e \@ "id"
            val lang = Some(e \@ "lang").filterNot(_.isEmpty())
            id -> (code, lang)
          }
        )
        .toMap

    val maths: Map[String, String] = (elem \ "predef" \ "math")
      .flatMap(es =>
        es.map { e =>
          val math = e.text.stripLeading()
          val id = e \@ "id"

          id -> math
        }
      )
      .toMap

    IO.pure(
      domain.model.Context(
        voiceConfigMap,
        characterConfigMap,
        defaultBackgroundImage,
        dict = dict,
        codes = codes,
        maths = maths,
        font = defaultFont
      )
    )
  }

  private def generateVideo(
      sayCtxPairs: Seq[(domain.model.Say, Context)],
      pathAndDurations: Seq[(fs2.io.file.Path, FiniteDuration)]
  ): IO[os.Path] = {
    import cats.syntax.parallel._
    val saySeq = sayCtxPairs map { case (s, ctx) =>
      for {
        stream <- buildHtmlFile(s.text, ctx).map(s =>
          fs2.Stream[IO, Byte](s.getBytes(): _*)
        )
        sha1Hex <- sha1HexCode(s.text.getBytes())
        htmlFile <- writeStreamToFile(
          stream,
          s"./artifacts/html/${sha1Hex}.html"
        )
        screenShotFile <- screenShot.takeScreenShot(
          os.pwd / os.RelPath(htmlFile.toString)
        )
      } yield screenShotFile
    }
    val sceneImages =
      saySeq.grouped(10).map(_.parSequence).toSeq.reduceRight[IO[Seq[Path]]] {
        case (acc, io) => acc.flatMap(acc => io.map(acc ++ _))
      }
    sceneImages.flatMap(imgs =>
      ffmpeg.concatenateImagesWithDuration(imgs.zip(pathAndDurations.map(_._2)))
    )
  }

  private def buildAudioQuery(
      text: String,
      character: String,
      voiceVox: VoiceVox,
      ctx: Context
  ) = {
    val characterConfig = ctx.characterConfigMap(character)
    val voiceConfig = ctx.voiceConfigMap(characterConfig.voiceId)
    // VOICEVOX特有の実装 いずれどこかの層に分離する
    val speakerId =
      voiceConfig.asInstanceOf[domain.model.VoiceVoxBackendConfig].speakerId
    voiceVox.audioQuery(text, speakerId)
  }

  private def buildWavFile(
      aq: AudioQuery,
      character: String,
      voiceVox: VoiceVox,
      ctx: Context
  ): IO[fs2.Stream[IO, Byte]] = {
    val characterConfig = ctx.characterConfigMap(character)
    val voiceConfig = ctx.voiceConfigMap(characterConfig.voiceId)
    // VOICEVOX特有の実装 いずれどこかの層に分離する
    val speakerId =
      voiceConfig.asInstanceOf[domain.model.VoiceVoxBackendConfig].speakerId
    voiceVox.synthesis(aq, speakerId)
  }

  // TODO: Templaceコンポーネントとかに切り出す
  private def buildHtmlFile(serif: String, ctx: Context): IO[String] = {
    IO { html.sample(serif = serif, ctx = ctx).body }
  }

  private def withColor(color: String) = (s: String) =>
    s"${color.toString()}${s}${scala.io.AnsiColor.RESET}"

  // 進捗インジケータを表示するためのユーティリティ
  private def backgroundIndicator(
      message: String
  ): cats.effect.ResourceIO[IO[cats.effect.OutcomeIO[Unit]]] =
    indicator(message).background
  import concurrent.duration._
  import scala.language.postfixOps
  private def piece(s: String): IO[Unit] =
    IO.sleep(100 milliseconds) *> IO.print(
      s"\r${withColor(scala.io.AnsiColor.GREEN ++ scala.io.AnsiColor.BOLD)(s)}"
    )
  private def indicator(message: String): IO[Unit] =
    piece(s"⢄ $message") *> piece(s"⠢ $message") *> piece(
      s"⠑ $message"
    ) *> piece(s"⡈ $message") foreverM

}
