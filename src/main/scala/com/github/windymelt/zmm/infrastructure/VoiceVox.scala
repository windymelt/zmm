package com.github.windymelt.zmm
package infrastructure

trait VoiceVoxComponent {
  self: domain.repository.VoiceVoxComponent =>

  import cats.effect.IO
  import org.http4s.ember.client._
  import org.http4s.client._
  import org.http4s.Request
  import org.http4s.Method
  import org.http4s.Headers
  import org.http4s.Uri
  import io.circe._
  import io.circe.literal._
  import org.http4s.circe.CirceEntityDecoder._

  type AudioQuery = Json // TODO: 必要に応じて高級なcase class / HListにする
  type SpeakerInfo = Json // TODO: 必要に応じて高級なcase class / HListにする
  def voiceVox: VoiceVox

  /** VOICEVOX client.
    *
    * You can start local VOICEVOX container with Docker:
    * {{{
    * docker run --rm -it -p '127.0.0.1:50021:50021' voicevox/voicevox_engine:cpu-ubuntu20.04-latest
    * }}}
    */
  class ConcreteVoiceVox extends VoiceVox {
    def speakers(): IO[SpeakerInfo] = client.use {
      c =>
      val req = Request[IO](uri = Uri.fromString("http://localhost:50021/speakers").right.get)
      c.expect[SpeakerInfo](req)
    }
    // TODO: localhost:50021決め打ちをやめる
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

    def controlSpeed(aq: AudioQuery, speed: String): IO[AudioQuery] = {
      import io.circe.syntax._
      // TODO: .getやめて失敗できるようにする
      IO.pure(aq.hcursor.downField("speedScale").withFocus(_ => speed.asJson).top.get)
    }

    private lazy val client = {
      import concurrent.duration._
      import scala.language.postfixOps
      EmberClientBuilder.default[IO].withTimeout(5 minutes).withIdleConnectionTime(5 minutes).build
    }
  }
}
