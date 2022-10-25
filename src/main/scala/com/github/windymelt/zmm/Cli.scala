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
          voiceVox = new ConcreteVoiceVox()
          _ <- IO.println(ctx)
          paths <- {
            import cats.implicits._
            import cats.effect.implicits._
            val saySeq = (x \ "dialogue" \ "say").map(say =>
              generateSay(say, voiceVox, ctx)
            )
            saySeq.parSequence
          }
          _ <- IO.println(s"Wrote to $paths")
          // TODO: 最終的に適当にスペースを挟んだ1つのwavになってほしい
          _ <- ffmpeg.concatenateWavFiles(paths.map(_.toString))
        } yield ()
      } >>
      IO.unit
  }

  private def generateSay(
      sayElem: scala.xml.Node,
      voiceVox: VoiceVox,
      ctx: domain.model.Context
  ): IO[fs2.io.file.Path] = for {
    aq <- buildAudioQuery(sayElem.text, sayElem \@ "by", voiceVox, ctx)
//    _ <- IO.println(aq)
    fixedAq <- sayElem.attribute("speed") map (attrNode =>
      voiceVox.controlSpeed(aq, attrNode.text)
    ) getOrElse (IO.pure(aq))
    wav <- buildWavFile(fixedAq, sayElem \@ "by", voiceVox, ctx)
    fileName = sayElem.text.replaceAll(
      "\n",
      ""
    ) // ffmpegに渡すときに困らないようにいったん改行を削除している。あとで機械的な名前に変更する
    sha1Hex = sha1HexCode(fileName.getBytes())
    path <- writeToFile(wav, s"artifacts/voice_${sha1Hex}.wav")
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
}
