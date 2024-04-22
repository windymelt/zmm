package com.github.windymelt.zmm

import domain.repository.ScreenShot
import cats.data.EitherT
import cats.effect.kernel.Resource
import cats.effect.IO
import com.github.windymelt.zmm.domain.model.{
  Context,
  SilentBackendConfig,
  VoiceBackendConfig,
  VoiceVoxBackendConfig,
}
import com.github.windymelt.zmm.domain.repository.VoiceVox
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import concurrent.duration.FiniteDuration

class Cli(
    voiceVox: domain.repository.VoiceVox,
    ffmpeg: domain.repository.FFmpeg,
    screenShotResource: IO[Resource[IO, ScreenShot]],
) {

  val zmmLogo = """ _________  ______  ___
|___  /|  \/  ||  \/  |
   / / | .  . || .  . |
  / /  | |\/| || |\/| |
./ /___| |  | || |  | |
\_____/\_|  |_/\_|  |_/"""

  implicit def logger: Logger[IO] = Slf4jLogger.getLogger[IO]

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
              s("id").get.asNumber.get.toString,
            )(s("name").get.asString.get),
          )
        }
        speakersArray.flatMap(speakerToSeq).map(_.toArray).toArray
      }
      _ <- IO.println(
        ASCIITable.fromData(
          Seq("voice", "voice ID", "style").toArray,
          speakersTable,
        ),
      )
    } yield ()
  }

  def generate(
      filePath: String,
      outPathString: String,
  ): EitherT[IO, String, Unit] = {
    import cats.syntax.all.{*, given}
    import scala.util.control.Exception.allCatch

    val content = EitherT(
      IO.delay(
        allCatch
          .either(
            scala.xml.XML.loadFile(filePath),
          ).leftMap(_.getMessage),
      ),
    )

    for {
      _ <- EitherT.right(logger.debug(s"generate($filePath, $outPathString)"))
      _ <- EitherT.right(showLogo)
      _ <- EitherT.right(
        logger.debug(s"pwd: ${System.getProperty("user.dir")}"),
      )
      _ <- EitherT.right(logger.debug(s"voicevox api: ${voiceVox.voiceVoxUri}"))
      _ <- EitherT.right(
        logger.debug(
          s"""ffmpeg command: ${ffmpeg.ffmpegCommand}""",
        ),
      )
      x <- content
      _ <- contentSanityCheck(x)
      defaultCtx <- prepareDefaultContext(x)
      _ <- applyDictionary(defaultCtx)
      sayCtxPairs <- EitherT.rightT(
        Context.fromNode((x \ "dialogue").head, defaultCtx),
      )
      voices <- {
        import cats.syntax.parallel._
        val saySeq = sayCtxPairs map {
          case (s, ctx)
              if ctx.spokenByCharacterId == Some(
                "silent",
              ) => // TODO: voiceconfigまで辿る
            generateSilence(ctx)
          case (s, ctx) =>
            generateSay(s, voiceVox, ctx)
        }
        EitherT.right(saySeq.parSequence)
      }
      // 読み上げ長をContextに追加する。母音情報が得られた場合も追加する
      sayCtxPairs <- EitherT.rightT {
        val pairs = sayCtxPairs zip voices
        pairs map {
          case ((say, context), (_, dur, Seq())) =>
            (say, context.copy(duration = Some(dur)))
          case ((say, context), (_, dur, vowels)) =>
            (
              say,
              context.copy(spokenVowels = Some(vowels), duration = Some(dur)),
            )
        }
      }
      // Contextにフィルタを適用する
      sayCtxPairs <- EitherT.rightT(applyFilters(sayCtxPairs))
      // この時点でvideoとaudioとの間に依存がないので並列実行する
      // BUG: SI-5589 により、タプルにバインドできない
      (video, audio) <- EitherT.right(
        backgroundIndicator("Generating video and concatenated audio").use {
          _ =>
            val paths = voices.map(_._1)
            generateVideo(sayCtxPairs, paths) product ffmpeg
              .concatenateWavFiles(paths.map(_.toString))
        },
      )
      zippedVideo <- EitherT.right(
        backgroundIndicator("Zipping silent video and audio").use { _ =>
          ffmpeg.zipVideoWithAudio(video, audio)
        },
      )
      composedVideo <- EitherT.right(
        backgroundIndicator("Composing Video").surround {
          import util.Util.EqForPath

          // もし設定されていればビデオを合成する。BGMと同様、同じビデオであれば結合する。
          val videoWithDuration: Seq[(Option[os.Path], FiniteDuration)] =
            sayCtxPairs
              .map(p =>
                p._2.video.map(path =>
                  os.pwd / os.RelPath(util.PathAlias.resolve(path, "ffmpeg")),
                ) -> p._2.duration.get,
              )

          val reductedVideoWithDuration =
            util.Util.groupReduction(videoWithDuration)

          // 環境によっては上書きに失敗する？ので出力ファイルが存在する場合削除する
          val outputFile = os.pwd / "output_composed.mp4"
          os.remove(outputFile, checkExists = false)

          reductedVideoWithDuration.filter(_._1.isDefined).size match {
            case 0 =>
              IO.delay {
                os.move(zippedVideo, outputFile)
                outputFile
              }
            case _ =>
              ffmpeg.composeVideoWithDuration(
                zippedVideo,
                reductedVideoWithDuration,
              )
          }
        },
      )
      _ <- EitherT.right(backgroundIndicator("Applying BGM").use { _ =>
        import util.Util.EqForPath

        // BGMを合成する。BGMはコンテキストで割り当てる。sayCtxPairsでsayごとにコンテキストが確定するので、同じBGMであれば結合しつつ最終的なDurationを計算する。
        // たとえば、BGMa 5sec BGMa 5sec BGMb 10sec であるときは、 BGMa 10sec BGMb 10secに簡約される。
        val bgmWithDuration: Seq[(Option[os.Path], FiniteDuration)] =
          sayCtxPairs
            .map(p =>
              p._2.bgm.map(path =>
                os.pwd / os.RelPath(util.PathAlias.resolve(path, "ffmpeg")),
              ) -> p._2.duration.get,
            )

        val reductedBgmWithDuration = util.Util.groupReduction(bgmWithDuration)

        // 環境によっては上書きに失敗する？ので出力ファイルが存在する場合削除する
        val outputFilePath = os.Path(outPathString)
        os.remove(outputFilePath, checkExists = false)

        reductedBgmWithDuration.filter(_._1.isDefined).size match {
          case 0 =>
            IO.pure(
              os.move(composedVideo, outputFilePath),
            ) // Dirty fix. TODO: fix here
          case _ =>
            ffmpeg.zipVideoWithAudioWithDuration(
              composedVideo,
              reductedBgmWithDuration,
              outputFilePath,
            )
        }
      })
      _ <- EitherT.rightT(logger.info(s"Done! Generated to $outPathString"))
    } yield Right(())
  }

  private def applyFilters(
      pairs: Seq[(domain.model.Say, Context)],
  ): Seq[(domain.model.Say, Context)] = {
    // フィルタが増えたら合成して伸ばす
    val composedFilters = domain.model.Filter.talkingMouthFilter
    // Arrow.secondを使うとタプルの右側だけflatMapし、左側を補完させることができる
    pairs.flatMap(composedFilters.second.run)
  }

  private def showLogo: IO[Unit] =
    IO.println(
      withColor(scala.io.AnsiColor.GREEN ++ scala.io.AnsiColor.BOLD)(zmmLogo),
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
  private def applyDictionary(ctx: Context): EitherT[IO, String, Unit] =
    EitherT.right {
      import cats.syntax.parallel._
      val registerList = ctx.dict.map { d =>
        voiceVox.registerDict(d._1, d._2, d._3)
      }
      registerList.reduceLeft[IO[Unit]] { case (acc, i) => i >> acc }
    }

  private def generateSay(
      sayElem: domain.model.Say,
      voiceVox: VoiceVox,
      ctx: Context,
  ): IO[
    (
        fs2.io.file.Path,
        scala.concurrent.duration.FiniteDuration,
        domain.model.VowelSeqWithDuration,
    ),
  ] = for {
    actualPronunciation <- IO.pure(
      ctx.sic.getOrElse(sayElem.text),
    ) // sicがない場合は元々のセリフを使う
    aq <- backgroundIndicator("Building Audio Query").use { _ =>
      // by属性がないことはないやろという想定でgetしている
      buildAudioQuery(
        actualPronunciation,
        ctx.spokenByCharacterId.get,
        voiceVox,
        ctx,
      )
    }
    _ <- logger.debug(aq.toString())
    aq <- ctx.speed map (sp => voiceVox.controlSpeed(aq, sp)) getOrElse (IO
      .pure(aq))
    wav <- backgroundIndicator("Synthesizing wav").use { _ =>
      buildWavFile(aq, ctx.spokenByCharacterId.get, voiceVox, ctx)
    }
    sha1Hex <- util.Util.sha1HexCode(sayElem.text.getBytes())
    path <- backgroundIndicator("Exporting .wav file").use { _ =>
      util.Util.writeStreamToFile(wav, s"artifacts/voice_${sha1Hex}.wav")
    }
    dur <- ffmpeg.getWavDuration(path.toString)
    vowels <- voiceVox.getVowels(aq)
  } yield (path, dur, vowels)

  private def generateSilence(
      ctx: Context,
  ): IO[(fs2.io.file.Path, FiniteDuration, domain.model.VowelSeqWithDuration)] =
    for {
      len <- IO.pure(
        ctx.silentLength.getOrElse(FiniteDuration(3, "second")),
      ) // 指定してないなら3秒にしているが理由はない
      sha1Hex <- util.Util.sha1HexCode(len.toString.getBytes)
      path <- IO.pure(os.Path(s"${os.pwd}/artifacts/silence_$sha1Hex.wav"))
      wav <- backgroundIndicator("Exporting silent .wav file").use { _ =>
        ffmpeg.generateSilentWav(path, len)
      }
    } yield (fs2.io.file.Path(path.toString()), len, Seq())

  private def contentSanityCheck(
      elem: scala.xml.Elem,
  ): EitherT[IO, String, Unit] = {
    val checkTopElem = elem.label == "content"
    val ver = elem \@ "version" == "0.0"

    if (!(checkTopElem && ver)) {
      EitherT.leftT("Invalid scenary XML") // TODO: 丁寧なエラーメッセージ
    } else {
      EitherT.rightT(())
    }
  }

  private def prepareDefaultContext(
      elem: scala.xml.Elem,
  ): EitherT[IO, String, Context] = EitherT.right {
    val voiceConfigList = elem \ "meta" \ "voiceconfig"
    val voiceConfigMap: Map[String, VoiceBackendConfig] = voiceConfigList.map {
      vc =>
        vc \@ "backend" match {
          case "voicevox" =>
            val vvc = vc \ "voicevoxconfig"
            val voiceVoxSpeakerId = vvc \@ "id"
            (vc \@ "id", domain.model.VoiceVoxBackendConfig(voiceVoxSpeakerId))
          case "silent" =>
            (vc \@ "id", domain.model.SilentBackendConfig())
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
        tachieUrl,
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
    val dict: Seq[(String, String, Int)] = util.Dict.dictFromNode(elem)

    val codes: Map[String, (String, Option[String])] =
      (elem \ "predef" \ "code")
        .flatMap(es =>
          es.map { e =>
            val code = e.text.stripLeading()
            val id = e \@ "id"
            val lang = Some(e \@ "lang").filterNot(_.isEmpty())
            id -> (code, lang)
          },
        )
        .toMap

    val maths: Map[String, String] = (elem \ "predef" \ "math")
      .flatMap(es =>
        es.map { e =>
          val math = e.text.stripLeading()
          val id = e \@ "id"

          id -> math
        },
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
        font = defaultFont,
      ),
    )
  }

  private def generateVideo(
      sayCtxPairs: Seq[(domain.model.Say, Context)],
      paths: Seq[fs2.io.file.Path],
  ): IO[os.Path] = {
    import cats.syntax.parallel._

    val fileCheck: String => IO[Boolean] = p =>
      IO(os.exists(os.pwd / os.RelPath(p)))

    // スクリーンショットは重いのでHTMLの内容をもとにキャッシュする(HTMLが同一内容なら同一のスクリーンショットになるという前提)
    val shot: ScreenShot => (domain.model.Say, Context) => IO[os.Path] =
      (ss: ScreenShot) =>
        (s: domain.model.Say, ctx: Context) => {
          val htmlIO = buildHtmlFile(s.text, ctx)
          for {
            stream <- htmlIO.map(s => fs2.Stream[IO, Byte](s.getBytes().toSeq*))
            html <- htmlIO
            sha1Hex <- util.Util.sha1HexCode(html.getBytes())
            htmlPath = s"./artifacts/html/${sha1Hex}.html"
            htmlFile <- fileCheck(htmlPath).ifM(
              IO.pure(fs2.io.file.Path(htmlPath)),
              util.Util.writeStreamToFile(stream, htmlPath),
            )
            _ <- fileCheck(s"${htmlPath}.png").ifM(
              logger.debug(s"Cache HIT: ${htmlPath}.png"),
              logger.debug(s"Cache expired: ${htmlPath}.png"),
            )
            screenShotFile <- fileCheck(s"${htmlPath}.png").ifM(
              IO.pure(
                os.pwd / os.RelPath(s"${htmlPath}.png"),
              ),
              ss.takeScreenShot(
                os.pwd / os.RelPath(htmlFile.toString),
              ),
            )
          } yield screenShotFile
        }

    for {
      ss <- screenShotResource
      imgs <- for {
        sceneImages <- sayCtxPairs.map { pair =>
          ss.use { ss => shot(ss).tupled(pair) }
        }.parSequence
        concatenatedImages <- ffmpeg.concatenateImagesWithDuration(
          sceneImages.zip(sayCtxPairs.map(_._2.duration.get)),
        )
      } yield concatenatedImages

    } yield imgs
  }

  private def buildAudioQuery(
      text: String,
      character: String,
      voiceVox: VoiceVox,
      ctx: Context,
  ) = {
    val characterConfig = ctx.characterConfigMap(character)
    val voiceConfig = ctx.voiceConfigMap(characterConfig.voiceId)
    // VOICEVOX特有の実装 いずれどこかの層に分離する
    voiceConfig match
      case domain.model.VoiceVoxBackendConfig(speakerId) =>
        voiceVox.audioQuery(text, speakerId)
  }

  private def buildWavFile(
      aq: domain.repository.AudioQuery,
      character: String,
      voiceVox: VoiceVox,
      ctx: Context,
  ): IO[fs2.Stream[IO, Byte]] = {
    val characterConfig = ctx.characterConfigMap(character)
    val voiceConfig = ctx.voiceConfigMap(characterConfig.voiceId)
    // VOICEVOX特有の実装 いずれどこかの層に分離する
    voiceConfig match
      case VoiceVoxBackendConfig(speakerId) =>
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
      message: String,
  ): cats.effect.ResourceIO[IO[cats.effect.OutcomeIO[Unit]]] =
    indicator(message).background
  import concurrent.duration._
  import scala.language.postfixOps
  private def piece(s: String): IO[Unit] =
    IO.sleep(100 milliseconds) *> IO.print(
      s"\r${withColor(scala.io.AnsiColor.GREEN ++ scala.io.AnsiColor.BOLD)(s)}",
    )
  private def indicator(message: String): IO[Unit] =
    piece(s"⢄ $message") *> piece(s"⠢ $message") *> piece(
      s"⠑ $message",
    ) *> piece(s"⡈ $message") foreverM

}
