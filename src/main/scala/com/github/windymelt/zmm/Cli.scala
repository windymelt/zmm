package com.github.windymelt.zmm

import domain.repository.ScreenShot
import com.github.windymelt.zmm.domain.model.{
  Context,
  SilentBackendConfig,
  VoiceBackendConfig,
  VoiceVoxBackendConfig,
}
import com.github.windymelt.zmm.domain.repository.VoiceVox
import zio.Console
import zio.Scope
import zio.Task
import zio.UIO
import zio.ZIO
import zio.durationInt

import concurrent.duration.FiniteDuration

/** スクリーンショット実装を排他制御付きで貸し出すサービス。
  *
  * スクリーンショットバックエンドは同時に1つしか起動できないため、Semaphoreで直列化する。
  */
final case class ScreenShotService(
    sem: zio.Semaphore,
    make: () => ScreenShot,
):
  def acquire: ZIO[Scope, Nothing, ScreenShot] =
    sem.withPermitScoped.as(make())

class Cli(
    voiceVox: domain.repository.VoiceVox,
    ffmpeg: domain.repository.FFmpeg,
    screenShot: ScreenShotService,
) {

  val zmmLogo = """ _________  ______  ___
|___  /|  \/  ||  \/  |
   / / | .  . || .  . |
  / /  | |\/| || |\/| |
./ /___| |  | || |  | |
\_____/\_|  |_/\_|  |_/"""

  def showVoiceVoxSpeakers(): Task[Unit] = {
    import io.circe.JsonObject
    import com.mitchtalmadge.asciidata.table.ASCIITable
    for {
      speakers <- voiceVox.speakers()
      speakersTable <- ZIO.attempt {
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
      _ <- Console.printLine(
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
  ): Task[Unit] = {
    val content = ZIO.attempt(
      scala.xml.XML.loadFile(filePath),
    )

    for {
      _ <- ZIO.logDebug(s"generate($filePath, $outPathString)")
      _ <- showLogo
      _ <- ZIO.logDebug(s"pwd: ${System.getProperty("user.dir")}")
      _ <- ZIO.logDebug(s"voicevox api: ${voiceVox.voiceVoxUri}")
      _ <- ZIO.logDebug(s"""ffmpeg command: ${ffmpeg.ffmpegCommand}""")
      x <- content
      _ <- contentSanityCheck(x)
      defaultCtx <- prepareDefaultContext(x)
      _ <- applyDictionary(defaultCtx)
      sayCtxPairs <- ZIO.attempt(
        Context.fromNode((x \ "dialogue").head, defaultCtx),
      )
      voices <- {
        val saySeq = sayCtxPairs map:
          case (s, ctx)
              if ctx.spokenByCharacterId == Some(
                "silent",
              ) => // TODO: voiceconfigまで辿る
            generateSilence(ctx)
          case (s, ctx) =>
            generateSay(s, voiceVox, ctx)
        // VOICEVOXエンジン側が詰まらないよう並列度に上限を設ける
        ZIO.withParallelism(4)(ZIO.collectAllPar(saySeq))
      }
      // 読み上げ長をContextに追加する。母音情報が得られた場合も追加する
      sayCtxPairs <- ZIO.attempt {
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
      sayCtxPairs <- ZIO.attempt(applyFilters(sayCtxPairs))
      // この時点でvideoとaudioとの間に依存がないので並列実行する
      // BUG: SI-5589 により、タプルにバインドできない
      (video, audio) <- backgroundIndicator(
        "Generating video and concatenated audio",
      ) {
        val paths = voices.map(_._1)
        generateVideo(sayCtxPairs) zipPar ffmpeg
          .concatenateWavFiles(paths.map(_.toString))
      }
      zippedVideo <- backgroundIndicator("Zipping silent video and audio") {
        ffmpeg.zipVideoWithAudio(video, audio)
      }
      composedVideo <- backgroundIndicator("Composing Video") {
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

        ZIO.attemptBlocking(os.remove(outputFile, checkExists = false)) *>
          (reductedVideoWithDuration.filter(_._1.isDefined).size match {
            case 0 =>
              ZIO.attemptBlocking {
                os.move(zippedVideo, outputFile)
                outputFile
              }
            case _ =>
              ffmpeg.composeVideoWithDuration(
                zippedVideo,
                reductedVideoWithDuration,
              )
          })
      }
      _ <- backgroundIndicator("Applying BGM") {
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

        ZIO.attemptBlocking(os.remove(outputFilePath, checkExists = false)) *>
          (reductedBgmWithDuration.filter(_._1.isDefined).size match {
            case 0 =>
              ZIO.attemptBlocking(
                os.move(composedVideo, outputFilePath),
              ) // Dirty fix. TODO: fix here
            case _ =>
              ffmpeg.zipVideoWithAudioWithDuration(
                composedVideo,
                reductedBgmWithDuration,
                outputFilePath,
              )
          })
      }
      _ <- ZIO.logInfo(s"Done! Generated to $outPathString")
    } yield ()
  }

  private def applyFilters(
      pairs: Seq[(domain.model.Say, Context)],
  ): Seq[(domain.model.Say, Context)] = {
    // フィルタが増えたら合成して伸ばす
    val composedFilters = domain.model.Filter.talkingMouthFilter
    // Arrow.secondを使うとタプルの右側だけflatMapし、左側を補完させることができる
    pairs.flatMap(composedFilters.second.run)
  }

  private def showLogo: Task[Unit] =
    Console.printLine(
      withColor(scala.io.AnsiColor.GREEN ++ scala.io.AnsiColor.BOLD)(zmmLogo),
    ) *>
      Console.printLine(
        withColor(scala.io.AnsiColor.GREEN)(s"${BuildInfo.version}"),
      )

  /** ZMMのバージョンを表示する。
    *
    * デバッグや問い合わせの助けとしても使う。
    *
    * @return
    *   Task[Unit]
    */
  def showVersion: Task[Unit] =
    Console.print("zmm ver=") *>
      (BuildInfo.version match {
        case s"$_-SNAPSHOT" =>
          Console.print(withColor(scala.io.AnsiColor.YELLOW)(BuildInfo.version))
        case _ =>
          Console.print(withColor(scala.io.AnsiColor.GREEN)(BuildInfo.version))
      }) *>
      ((System.getenv("IS_DOCKER_ZMM") == "1") match {
        case true =>
          Console.print(withColor(scala.io.AnsiColor.CYAN)(" (Docker)"))
        case false => ZIO.unit
      }) *>
      Console.print(", scalaVer=") *>
      Console.print(
        withColor(scala.io.AnsiColor.GREEN)(BuildInfo.scalaVersion),
      ) *>
      Console.print(", sbtVer=") *>
      Console.print(withColor(scala.io.AnsiColor.GREEN)(BuildInfo.sbtVersion)) *>
      Console.print(s", jvm=${System.getProperty("java.vm.name")}") *>
      Console.print(s", runtimeVer=${Runtime.version().toString()}") *>
      Console.print(s", vendor=${System.getProperty("java.vendor")}") *>
      Console.printLine("")

  /** 辞書要素を反映させる。
    *
    * 今のところVOICEVOX用の発音辞書に登録を行うだけだが、今後の開発によってはその他の音声合成ソフトウェアの辞書登録に使ってよい。
    *
    * @param ctx
    *   辞書を取り出す元となるコンテキスト
    * @return
    *   有用な情報は返されない
    */
  private def applyDictionary(ctx: Context): Task[Unit] =
    ZIO.foreachDiscard(ctx.dict): d =>
      voiceVox.registerDict(d._1, d._2, d._3)

  private def generateSay(
      sayElem: domain.model.Say,
      voiceVox: VoiceVox,
      ctx: Context,
  ): Task[
    (
        os.Path,
        scala.concurrent.duration.FiniteDuration,
        domain.model.VowelSeqWithDuration,
    ),
  ] = for {
    actualPronunciation <- ZIO.succeed(
      ctx.sic.getOrElse(sayElem.text),
    ) // sicがない場合は元々のセリフを使う
    aq <- backgroundIndicator("Building Audio Query") {
      // by属性がないことはないやろという想定でgetしている
      buildAudioQuery(
        actualPronunciation,
        ctx.spokenByCharacterId.get,
        voiceVox,
        ctx,
      )
    }
    _ <- ZIO.logDebug(aq.toString())
    aq <- ctx.speed map (sp => voiceVox.controlSpeed(aq, sp)) getOrElse (ZIO
      .succeed(aq))
    wav <- backgroundIndicator("Synthesizing wav") {
      buildWavFile(aq, ctx.spokenByCharacterId.get, voiceVox, ctx)
    }
    sha1Hex = util.Util.sha1HexCode(sayElem.text.getBytes())
    path <- backgroundIndicator("Exporting .wav file") {
      util.Util.writeBytesToFile(wav, s"artifacts/voice_${sha1Hex}.wav")
    }
    dur <- ffmpeg.getWavDuration(path.toString)
    vowels <- voiceVox.getVowels(aq)
  } yield (path, dur, vowels)

  private def generateSilence(
      ctx: Context,
  ): Task[(os.Path, FiniteDuration, domain.model.VowelSeqWithDuration)] =
    for {
      len <- ZIO.succeed(
        ctx.silentLength.getOrElse(FiniteDuration(3, "second")),
      ) // 指定してないなら3秒にしているが理由はない
      sha1Hex = util.Util.sha1HexCode(len.toString.getBytes)
      path = os.Path(s"${os.pwd}/artifacts/silence_$sha1Hex.wav")
      wav <- backgroundIndicator("Exporting silent .wav file") {
        ffmpeg.generateSilentWav(path, len)
      }
    } yield (path, len, Seq())

  private def contentSanityCheck(
      elem: scala.xml.Elem,
  ): Task[Unit] = {
    val checkTopElem = elem.label == "content"
    val ver = elem \@ "version" == "0.0"

    if (!(checkTopElem && ver)) {
      ZIO.fail(Exception("Invalid scenary XML")) // TODO: 丁寧なエラーメッセージ
    } else {
      ZIO.unit
    }
  }

  private def prepareDefaultContext(
      elem: scala.xml.Elem,
  ): Task[Context] = ZIO.attempt {
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

    domain.model.Context(
      voiceConfigMap,
      characterConfigMap,
      defaultBackgroundImage,
      dict = dict,
      codes = codes,
      maths = maths,
      font = defaultFont,
    )
  }

  private def generateVideo(
      sayCtxPairs: Seq[(domain.model.Say, Context)],
  ): Task[os.Path] = {
    val fileCheck: String => Task[Boolean] = p =>
      ZIO.attemptBlocking(os.exists(os.pwd / os.RelPath(p)))

    // スクリーンショットは重いのでHTMLの内容をもとにキャッシュする(HTMLが同一内容なら同一のスクリーンショットになるという前提)
    val shot: ScreenShot => (domain.model.Say, Context) => Task[os.Path] =
      (ss: ScreenShot) =>
        (s: domain.model.Say, ctx: Context) => {
          for {
            html <- buildHtmlFile(s.text, ctx)
            sha1Hex = util.Util.sha1HexCode(html.getBytes())
            htmlPath = s"artifacts/html/${sha1Hex}.html"
            htmlFile <- ZIO.ifZIO(fileCheck(htmlPath))(
              ZIO.succeed(os.Path(htmlPath, os.pwd)),
              util.Util.writeBytesToFile(html.getBytes(), htmlPath),
            )
            _ <- ZIO.ifZIO(fileCheck(s"${htmlPath}.png"))(
              ZIO.logDebug(s"Cache HIT: ${htmlPath}.png"),
              ZIO.logDebug(s"Cache expired: ${htmlPath}.png"),
            )
            screenShotFile <- ZIO.ifZIO(fileCheck(s"${htmlPath}.png"))(
              ZIO.succeed(
                os.pwd / os.RelPath(s"${htmlPath}.png"),
              ),
              ss.takeScreenShot(htmlFile),
            )
          } yield screenShotFile
        }

    for {
      sceneImages <- ZIO.collectAllPar(sayCtxPairs.map { pair =>
        ZIO.scoped(screenShot.acquire.flatMap(ss => shot(ss).tupled(pair)))
      })
      concatenatedImages <- ffmpeg.concatenateImagesWithDuration(
        sceneImages.zip(sayCtxPairs.map(_._2.duration.get)),
      )
    } yield concatenatedImages
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
  ): Task[Array[Byte]] = {
    val characterConfig = ctx.characterConfigMap(character)
    val voiceConfig = ctx.voiceConfigMap(characterConfig.voiceId)
    // VOICEVOX特有の実装 いずれどこかの層に分離する
    voiceConfig match
      case VoiceVoxBackendConfig(speakerId) =>
        voiceVox.synthesis(aq, speakerId)
  }

  // TODO: Templaceコンポーネントとかに切り出す
  private def buildHtmlFile(serif: String, ctx: Context): Task[String] = {
    ZIO.attempt { html.sample(serif = serif, ctx = ctx).body }
  }

  private def withColor(color: String) = (s: String) =>
    s"${color.toString()}${s}${scala.io.AnsiColor.RESET}"

  // 進捗インジケータを表示しながらbodyを実行するためのユーティリティ
  private def backgroundIndicator[R, E, A](
      message: String,
  )(body: ZIO[R, E, A]): ZIO[R, E, A] =
    ZIO.scoped[R](indicator(message).forkScoped *> body)

  private def piece(s: String): UIO[Unit] =
    ZIO.sleep(100.millis) *> Console
      .print(
        s"\r${withColor(scala.io.AnsiColor.GREEN ++ scala.io.AnsiColor.BOLD)(s)}",
      )
      .orDie

  private def indicator(message: String): UIO[Nothing] =
    (piece(s"⢄ $message") *> piece(s"⠢ $message") *> piece(
      s"⠑ $message",
    ) *> piece(s"⡈ $message")).forever

}
