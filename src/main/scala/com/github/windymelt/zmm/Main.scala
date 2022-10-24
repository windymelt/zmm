package com.github.windymelt.zmm

import cats.effect.IO
import cats.effect.IOApp
import cats.effect.ExitCode
import java.io.OutputStream
import org.http4s.syntax.header

object Main extends IOApp with VoiceVoxComponent {
  def run(args: List[String]) = {
    // load source file
    val filePath = args(0)
    val content = IO.delay(scala.xml.XML.loadFile(filePath))

    IO.println("Hello Zundamon!") >>
    IO.println("Invoking audio api...") >>
    {
      for {
        x <- content
        _ <- contentSanityCheck(x)
        ctx <- prepareContext(x)
        voiceVox = new ConcreteVoiceVox(ctx)
        _ <- IO.println(ctx)
        // TODO: 今はテキストを直接取り出して直接ずんだもんに決め打ちしているが、ちゃんとcharacterconfigやvoiceconfigを見るようにする
        aq <- buildAudioQuery((x \\ "say").head.text, voiceVox, ctx)
        wav <- buildWavFile(aq, voiceVox, ctx)
        path <- writeToFile(wav)
        _ <- IO.println(s"Wrote to $path")
      } yield ()
    } >>
    IO.pure(ExitCode.Success)
  }

  private def contentSanityCheck(elem: scala.xml.Elem): IO[Unit] = {
    val checkTopElem = elem.label == "content"
    val ver = elem \@ "version" == "0.0"

    if (! (checkTopElem && ver)) {
      throw new Exception("Invalid scenary XML") // TODO: 丁寧なエラーメッセージ
    }
    IO.unit
  }

  private def prepareContext(elem: scala.xml.Elem): IO[Context] = {
    val voiceConfigList = elem \ "meta" \ "voiceconfig"
    val voiceConfigMap = voiceConfigList.map { vc =>
      vc \@ "backend" match {
        case "voicevox" =>
          val vvc = vc \ "voicevoxconfig"
          val voiceVoxSpeakerId = vvc \@ "id"
          (vc \@ "id", VoiceVoxBackendConfig(voiceVoxSpeakerId))
        case _ => ??? // not implemented
      }
    }.toMap

    IO.pure(Context(voiceConfigMap))
  }

  private def buildAudioQuery(text: String, voiceVox: VoiceVox, ctx: Context) = {
    val voiceConfigurationForCharacter = /* TODO: chararcter->voiceconfigの対応を取る */ ctx.voiceConfigMap.get("zundamon").map(_.asInstanceOf[VoiceVoxBackendConfig].speakerId).get
    voiceVox.audioQuery(text, voiceConfigurationForCharacter)
  }

  private def buildWavFile(aq: io.circe.Json, voiceVox: VoiceVox, ctx: Context): IO[fs2.Stream[IO, Byte]] = {
    val voiceConfigurationForCharacter = /* TODO: chararcter->voiceconfigの対応を取る */ ctx.voiceConfigMap.get("zundamon").map(_.asInstanceOf[VoiceVoxBackendConfig].speakerId).get
    voiceVox.synthesis(aq, voiceConfigurationForCharacter)
  }

  private def writeToFile(stream: fs2.Stream[IO, Byte]) = {
    import fs2.io.file.{Files, Path}
    val target = Path("zundamon.wav")
    stream.through(Files[IO].writeAll(target)).compile.drain.as(target)
  }
}

sealed trait VoiceBackendConfig
final case class VoiceVoxBackendConfig(speakerId: String) extends VoiceBackendConfig
case class Context(
  voiceConfigMap: Map[String, VoiceBackendConfig]
)

// TODO: あとでちゃんとしたCake Patternにする
trait VoiceVoxComponent {
  import org.http4s.ember.client._
  import org.http4s.client._
  import org.http4s.Request
  import org.http4s.Method
  import org.http4s.Headers
  import org.http4s.Uri
  import io.circe._
  import io.circe.literal._
  import org.http4s.circe.CirceEntityDecoder._

  final class ConcreteVoiceVox(val ctx: Context) extends VoiceVox {}

  /** VOICEVOX client.
    *
    * You can start local VOICEVOX container with Docker:
    * {{{
    * docker run --rm -it -p '127.0.0.1:50021:50021' voicevox/voicevox_engine:cpu-ubuntu20.04-latest
    * }}}
    */
  trait VoiceVox {
    type AudioQuery = Json // TODO: 必要に応じて高級なcase class / HListにする
    def audioQuery(text: String, speaker: String): IO[AudioQuery] = client.use { c =>
      val uri = Uri.fromString("http://localhost:50021/audio_query").map(
        _.copy(query = org.http4s.Query.fromMap(Map("speaker" -> Seq(speaker), "text" -> Seq(text))))
      )
      val req = Request[IO](Method.POST, uri = uri.right.get, headers = Headers("accept" -> "application/json"))
      c.expect[AudioQuery](req)
    }

    def synthesis(aq: AudioQuery, speaker: String): IO[fs2.Stream[IO, Byte]] = client.use { c =>
      val uri = Uri.fromString("http://localhost:50021/synthesis").map(
        _.copy(
          query = org.http4s.Query.fromMap(Map("speaker" -> Seq(speaker))),
        )
      )
      val req = Request[IO](Method.POST,
        uri = uri.right.get,
        headers = Headers("Content-Type" -> "application/json"),
        body = fs2.Stream.fromIterator[IO](aq.toString().getBytes().toIterator, 64) // TODO: chinksize適当に指定しているのでなんとかする
      )
      IO.pure(c.stream(req).flatMap(_.body))
    }

    private def client = EmberClientBuilder.default[IO].build
  }
}
