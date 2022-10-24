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

    content >>
    IO.println("Hello Zundamon!") >>
    IO.println("Invoking audio api...") >>
    buildAudioQuery >>
    {
      for {
        aq <- buildAudioQuery
        wav <- buildWavFile(aq)
        path <- writeToFile(wav)
        _ <- IO.println(s"Wrote to $path")
      } yield ()
    } >>
    IO.pure(ExitCode.Success)
  }

  private def buildAudioQuery = {
    voiceVox.audioQuery("こんにちはなのだ")
  }

  private def buildWavFile(aq: io.circe.Json): IO[fs2.Stream[IO, Byte]] = {
    voiceVox.synthesis(aq)
  }

  private def writeToFile(stream: fs2.Stream[IO, Byte]) = {
    import fs2.io.file.{Files, Path}
    val target = Path("zundamon.wav")
    stream.through(Files[IO].writeAll(target)).compile.drain.as(target)
  }
}

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

  val voiceVox = new ConcreteVoiceVox

  final class ConcreteVoiceVox extends VoiceVox {}

  trait VoiceVox {
    type AudioQuery = Json // TODO: 必要に応じて高級なcase class / HListにする
    def audioQuery(text: String): IO[AudioQuery] = client.use { c =>
      val uri = Uri.fromString("http://localhost:50021/audio_query").map(
        _.copy(query = org.http4s.Query.fromMap(Map("speaker" -> Seq("1"), "text" -> Seq(text))))
      )
      val req = Request[IO](Method.POST, uri = uri.right.get, headers = Headers("accept" -> "application/json"))
      c.expect[AudioQuery](req)
    }

    def synthesis(aq: AudioQuery): IO[fs2.Stream[IO, Byte]] = client.use { c =>
      val uri = Uri.fromString("http://localhost:50021/synthesis").map(
        _.copy(
          query = org.http4s.Query.fromMap(Map("speaker" -> Seq("1"))),
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
