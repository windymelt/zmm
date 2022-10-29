package com.github.windymelt.zmm

import cats.effect.IO
import cats.effect.IOApp
import cats.effect.ExitCode
import java.io.OutputStream
import org.http4s.syntax.header

final class Cli
    extends domain.repository.FFmpegComponent
    with domain.repository.VoiceVoxComponent
    with infrastructure.FFmpegComponent
    with infrastructure.VoiceVoxComponent
    with util.UtilComponent {

  def voiceVox: VoiceVox = new ConcreteVoiceVox()
  def ffmpeg = new ConcreteFFmpeg()

  def generate(filePath: String): IO[Unit] = {
    val content = IO.delay(scala.xml.XML.loadFile(filePath))

    IO.println("Hello Zundamon!") >>
      IO.println("Invoking audio api...") >> {
        for {
//        speakers <- voiceVox.speakers()
//        _ <- IO.println(speakers)
          x <- content
          _ <- contentSanityCheck(x)
          ctx <- prepareContext(x)
          //        _ <- IO.println(ctx)
          htmls <- {
            import cats.implicits._
            import cats.effect.implicits._
            val saySeq = (x \ "dialogue" \ "say").map(say =>
              for {
                stream <- buildHtmlFile(say.text).map(s => fs2.Stream[IO, Byte](s.getBytes(): _*))
                files <- writeToFile(stream, s"./artifacts/html/${say.text}.html")
              } yield ()
            )
            saySeq.parSequence
          }
          paths <- {
            import cats.implicits._
            import cats.effect.implicits._
            val saySeq = (x \ "dialogue" \ "say").map(say =>
              generateSay(say, voiceVox, ctx)
            )
            saySeq.parSequence
          }
          _ <- ffmpeg.concatenateWavFiles(paths.map(_.toString))
          _ <- IO.println("Done!")
        } yield ()
      } >>
      IO.unit
  }

  private def generateSay(
      sayElem: scala.xml.Node,
      voiceVox: VoiceVox,
      ctx: domain.model.Context
  ): IO[fs2.io.file.Path] = for {
    aq <- backgroundIndicator("Building Audio Query").use { _ =>
      buildAudioQuery(sayElem.text, sayElem \@ "by", voiceVox, ctx)
    }
//    _ <- IO.println(aq)
    fixedAq <- sayElem.attribute("speed") map (attrNode =>
      voiceVox.controlSpeed(aq, attrNode.text)
    ) getOrElse (IO.pure(aq))
    wav <- backgroundIndicator("Synthesizing wav").use { _ =>
      buildWavFile(fixedAq, sayElem \@ "by", voiceVox, ctx)
    }
    sha1Hex <- sha1HexCode(sayElem.text.getBytes())
    path <- backgroundIndicator("Exporting .wav file").use { _ =>
      writeToFile(wav, s"artifacts/voice_${sha1Hex}.wav")
    }
  } yield path

  private def contentSanityCheck(elem: scala.xml.Elem): IO[Unit] = {
    val checkTopElem = elem.label == "content"
    val ver = elem \@ "version" == "0.0"

    if (!(checkTopElem && ver)) {
      throw new Exception("Invalid scenary XML") // TODO: 丁寧なエラーメッセージ
    }
    IO.unit
  }

  private def prepareContext(elem: scala.xml.Elem): IO[domain.model.Context] = {
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
      name -> domain.model.CharacterConfig(name, cc \@ "voice-id")
    }.toMap

    IO.pure(domain.model.Context(voiceConfigMap, characterConfigMap))
  }

  private def buildAudioQuery(
      text: String,
      character: String,
      voiceVox: VoiceVox,
      ctx: domain.model.Context
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
      ctx: domain.model.Context
  ): IO[fs2.Stream[IO, Byte]] = {
    val characterConfig = ctx.characterConfigMap(character)
    val voiceConfig = ctx.voiceConfigMap(characterConfig.voiceId)
    // VOICEVOX特有の実装 いずれどこかの層に分離する
    val speakerId = voiceConfig.asInstanceOf[domain.model.VoiceVoxBackendConfig].speakerId
    voiceVox.synthesis(aq, speakerId)
  }

  private def writeToFile(stream: fs2.Stream[IO, Byte], fileName: String) = {
    import fs2.io.file.{Files, Path}
    val target = Path(fileName)
    stream.through(Files[IO].writeAll(target)).compile.drain.as(target)
  }

  // TODO: Templaceコンポーネントとかに切り出す
  private def buildHtmlFile(serif: String): IO[String] = {
    IO { html.sample(serif = serif).body }
  }

  // 進捗インジケータを表示するためのユーティリティ
  private def backgroundIndicator(message: String): cats.effect.ResourceIO[IO[cats.effect.OutcomeIO[Unit]]] =
    indicator(message).background
  import concurrent.duration._
  import scala.language.postfixOps
  private def piece(s: String): IO[Unit] = IO.sleep(100 milliseconds) *> IO.print(s"\r$s")
  private def indicator(message: String): IO[Unit] = piece(s"⢄ $message") *> piece(s"⠢ $message") *> piece(s"⠑ $message") *> piece(s"⡈ $message") foreverM

}
