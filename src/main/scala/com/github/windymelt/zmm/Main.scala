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

    val cli = new Cli()
    cli.generate(filePath) *>
      IO.pure(cats.effect.ExitCode.Success)
  }
}

sealed trait VoiceBackendConfig
final case class VoiceVoxBackendConfig(speakerId: String)
    extends VoiceBackendConfig

final case class CharacterConfig(name: String, voiceId: String)

case class Context(
    voiceConfigMap: Map[String, VoiceBackendConfig],
    characterConfigMap: Map[String, CharacterConfig]
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

  final class ConcreteVoiceVox extends VoiceVox {}

  /** VOICEVOX client.
    *
    * You can start local VOICEVOX container with Docker:
    * {{{
    * docker run --rm -it -p '127.0.0.1:50021:50021' voicevox/voicevox_engine:cpu-ubuntu20.04-latest
    * }}}
    */
  trait VoiceVox {
    // TODO: localhost:50021決め打ちをやめる
    type AudioQuery = Json // TODO: 必要に応じて高級なcase class / HListにする
    def audioQuery(text: String, speaker: String): IO[AudioQuery] = client.use {
      c =>
        val uri = Uri
          .fromString("http://localhost:50021/audio_query")
          .map(
            _.copy(query =
              org.http4s.Query
                .fromMap(Map("speaker" -> Seq(speaker), "text" -> Seq(text)))
            )
          )
        val req = Request[IO](
          Method.POST,
          uri = uri.right.get,
          headers = Headers("accept" -> "application/json")
        )
        c.expect[AudioQuery](req)
    }

    def synthesis(aq: AudioQuery, speaker: String): IO[fs2.Stream[IO, Byte]] =
      client.use { c =>
        val uri = Uri
          .fromString("http://localhost:50021/synthesis")
          .map(
            _.copy(
              query = org.http4s.Query.fromMap(Map("speaker" -> Seq(speaker)))
            )
          )
        val req = Request[IO](
          Method.POST,
          uri = uri.right.get,
          headers = Headers("Content-Type" -> "application/json"),
          body = fs2.Stream.fromIterator[IO](
            aq.toString().getBytes().toIterator,
            64
          ) // TODO: chinksize適当に指定しているのでなんとかする
        )
        IO.pure(c.stream(req).flatMap(_.body))
      }

    private lazy val client = EmberClientBuilder.default[IO].build
  }
}
