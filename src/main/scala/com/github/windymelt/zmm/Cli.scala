package com.github.windymelt.zmm

import cats.effect.IO
import cats.effect.IOApp
import cats.effect.ExitCode
import java.io.OutputStream
import org.http4s.syntax.header
import com.github.windymelt.zmm.domain.model.Context

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

  def voiceVox: VoiceVox = new ConcreteVoiceVox()
  def ffmpeg = new ConcreteFFmpeg(ConcreteFFmpeg.Quiet)
  def screenShot = new ChromeScreenShot("chromium", ChromeScreenShot.Quiet)

  def generate(filePath: String): IO[Unit] = {
    val content = IO.delay(scala.xml.XML.loadFile(filePath))

     for {
       _ <- showLogo
       _ <- IO.println("Invoking audio api...")
       //        speakers <- voiceVox.speakers()
       //        _ <- IO.println(speakers)
       x <- content
       _ <- contentSanityCheck(x)
       defaultCtx <- prepareDefaultContext(x)
       //        _ <- IO.println(ctx)
       sayCtxPairs <- IO.pure(Context.fromNode((x \ "dialogue").head, defaultCtx))
       pathAndDurations <- {
         import cats.implicits._
         import cats.effect.implicits._
         val saySeq = sayCtxPairs map { case (s, ctx) => generateSay(s, voiceVox, ctx) }
         saySeq.parSequence
       }
       video <- {
         import cats.implicits._
         import cats.effect.implicits._
         val saySeq = sayCtxPairs map { case (s, ctx) =>
           for {
             stream <- buildHtmlFile(s.text, ctx).map(s => fs2.Stream[IO, Byte](s.getBytes(): _*))
             sha1Hex <- sha1HexCode(s.text.getBytes())
             htmlFile <- writeStreamToFile(stream, s"./artifacts/html/${sha1Hex}.html")
             screenShotFile <- screenShot.takeScreenShot(os.pwd / os.RelPath(htmlFile.toString))
           } yield screenShotFile
         }
         val sceneImages = backgroundIndicator("Generating scenary image").use(_ => saySeq.parSequence)
         sceneImages.flatMap(imgs => ffmpeg.concatenateImagesWithDuration(imgs.zip(pathAndDurations.map(_._2))))
       }
       // 実装上の選択肢:
       // - 画像をwavと組み合わせてaviにしてから結合する
       // - wavのデータをもとに尺情報を組み立て、画像を一連のaviにしてからwavと合わせる
       // いったん個々の動画に変換する？
       audio <- backgroundIndicator("Concatenating wav files").use(_ => ffmpeg.concatenateWavFiles(pathAndDurations.map(_._1.toString)))
       _ <- backgroundIndicator("Zipping silent video and audio").use { _ => ffmpeg.zipVideoWithAudio(video, audio) }
       _ <- IO.println("\nDone!")
     } yield ()
}

  private def showLogo: IO[Unit] =
    IO.println(withColor(scala.io.AnsiColor.GREEN ++ scala.io.AnsiColor.BOLD)(zmmLogo)) >>
    IO.println(withColor(scala.io.AnsiColor.GREEN)(s"${BuildInfo.version}"))

  // TODO: sayやsceneごとにコンテキストを更新していくための処理
  private def composeContext(baseCtx: Context)(overlayCtx: Context): IO[Context] = ???
  private def extractContextFromNode(node: scala.xml.Node): IO[Context] = ???

  private def generateSay(
      sayElem: domain.model.Say,
      voiceVox: VoiceVox,
      ctx: Context
  ): IO[(fs2.io.file.Path, scala.concurrent.duration.FiniteDuration)] = for {
    aq <- backgroundIndicator("Building Audio Query").use { _ =>
      // by属性がないことはないやろという想定でgetしている
      buildAudioQuery(sayElem.text, ctx.spokenByCharacterId.get, voiceVox, ctx)
    }
//    _ <- IO.println(aq)
    fixedAq <- ctx.speed map (sp =>
      voiceVox.controlSpeed(aq, sp)
    ) getOrElse (IO.pure(aq))
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
      name -> domain.model.CharacterConfig(name, cc \@ "voice-id", defaultSerifColor)
    }.toMap

    val defaultBackgroundImage =
      (elem \ "meta" \ "assets" \ "backgroundImage")
        .filter(_.attribute("id").map(_.text).contains("default"))
        .headOption
        .flatMap(_.attribute("url").headOption.map(_.text))

    IO.pure(domain.model.Context(voiceConfigMap, characterConfigMap, defaultBackgroundImage))
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
    val speakerId = voiceConfig.asInstanceOf[domain.model.VoiceVoxBackendConfig].speakerId
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
    val speakerId = voiceConfig.asInstanceOf[domain.model.VoiceVoxBackendConfig].speakerId
    voiceVox.synthesis(aq, speakerId)
  }

  // TODO: Templaceコンポーネントとかに切り出す
  private def buildHtmlFile(serif: String, ctx: Context): IO[String] = {
    IO { html.sample(serif = serif, ctx = ctx).body }
  }

  private def withColor(color: String) = (s: String) => s"${color.toString()}${s}${scala.io.AnsiColor.RESET}"

  // 進捗インジケータを表示するためのユーティリティ
  private def backgroundIndicator(message: String): cats.effect.ResourceIO[IO[cats.effect.OutcomeIO[Unit]]] =
    indicator(message).background
  import concurrent.duration._
  import scala.language.postfixOps
  private def piece(s: String): IO[Unit] = IO.sleep(100 milliseconds) *> IO.print(s"\r${withColor(scala.io.AnsiColor.GREEN ++ scala.io.AnsiColor.BOLD)(s)}")
  private def indicator(message: String): IO[Unit] = piece(s"⢄ $message") *> piece(s"⠢ $message") *> piece(s"⠑ $message") *> piece(s"⡈ $message") foreverM

}
