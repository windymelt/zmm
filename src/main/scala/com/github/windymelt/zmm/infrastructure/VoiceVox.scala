package com.github.windymelt.zmm
package infrastructure

import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration

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
  class ConcreteVoiceVox(voiceVoxUri: String) extends VoiceVox {
    def speakers(): IO[SpeakerInfo] = client.use { c =>
      val req =
        Request[IO](uri =
          Uri.fromString(s"${voiceVoxUri}/speakers").fold(throw _, identity),
        )
      c.expect[SpeakerInfo](req)
    }
    // TODO: localhost:50021決め打ちをやめる
    def audioQuery(text: String, speaker: String): IO[AudioQuery] = client.use {
      c =>
        val uri = Uri
          .fromString(s"${voiceVoxUri}/audio_query")
          .map(
            _.copy(query =
              org.http4s.Query
                .fromMap(Map("speaker" -> Seq(speaker), "text" -> Seq(text))),
            ),
          )
        val req = Request[IO](
          Method.POST,
          uri = uri.fold(throw _, identity),
          headers = Headers("accept" -> "application/json"),
        )
        c.expect[AudioQuery](req)
    }

    def synthesis(aq: AudioQuery, speaker: String): IO[fs2.Stream[IO, Byte]] =
      client.use { c =>
        val uri = Uri
          .fromString(s"${voiceVoxUri}/synthesis")
          .map(
            _.copy(
              query = org.http4s.Query.fromMap(Map("speaker" -> Seq(speaker))),
            ),
          )
        val req = Request[IO](
          Method.POST,
          uri = uri.fold(throw _, identity),
          headers = Headers("Content-Type" -> "application/json"),
          body = fs2.Stream.fromIterator[IO](
            aq.toString().getBytes().iterator,
            64,
          ), // TODO: chinksize適当に指定しているのでなんとかする
        )
        IO.pure(c.stream(req).flatMap(_.body))
      }

    def controlSpeed(aq: AudioQuery, speed: String): IO[AudioQuery] = {
      import io.circe.syntax._
      // TODO: .getやめて失敗できるようにする
      IO.pure(
        aq.hcursor.downField("speedScale").withFocus(_ => speed.asJson).top.get,
      )
    }

    def registerDict(
        word: String,
        pronounce: String,
        lowerPoint: Int,
    ): IO[Unit] = client.use { c =>
      val uri = Uri
        .fromString(s"${voiceVoxUri}/user_dict_word")
        .map(
          _.copy(
            query = org.http4s.Query.fromMap(
              Map(
                "surface" -> Seq(word),
                "pronunciation" -> Seq(pronounce),
                "accent_type" -> Seq(lowerPoint.toString),
              ),
            ),
          ),
        )

      val req = Request[IO](
        Method.POST,
        uri = uri.fold(throw _, identity),
        headers = Headers("Content-Type" -> "application/json"),
      )

      c.successful(req) *> IO.unit
    }

    def getVowels(aq: AudioQuery): IO[domain.model.VowelSeqWithDuration] =
      IO.pure {
        import io.circe.parser._
        import io.circe.optics.JsonPath._
        import io.circe.syntax._
        import cats.data.{NonEmptySeq => NES}
        import cats.implicits._
        // 簡単のために母音と子音まとめて時間に含めてしまう

        // 母音
        val vowels: Seq[String] =
          root.accent_phrases.each.moras.each.vowel.string.getAll(aq)
        val vowelDurs: Seq[Double] =
          root.accent_phrases.each.moras.each.vowel_length.double.getAll(aq)

        // 子音はあったりなかったりするのでちょっと複雑
        // 複数のOpticsの合成で値を取り出す
        val moras =
          root.accent_phrases.each.moras.each.json.getAll(aq)
        val consonantDurs: Seq[Double] =
          moras
            .map(root.consonant_length.double.getOption)
            .map(_.getOrElse(0.0))
            .map {
              case d if d.isNaN => 0.0
              case d            => d
            }

        // 無音期間
        val accent_phrases = root.accent_phrases.each.json.getAll(aq)
        val pausesDur: Seq[Double] = accent_phrases
          .map(root.pause_mora.vowel_length.double.getOption)
          .map(
            _.getOrElse(0.0),
          ) // vowel_lengthがNaNになることはない(required)のでisNanは調べなくてよい

        // 2つのSeqをおなじ位置の要素同士足して1つのSeqにしたい。
        // Seqをアプリカティブに足すとデカルト積のように全要素を足し合わせる巨大なSeqになってしまう。
        // 同じ位置の要素同士を足すにはZipListを使う。
        // ZipListはNonEmptyList(Seq)とparallelの関係にあるので、2つのNonEmptySeqをparMapNして足せば完成する
        val durs: Seq[Double] =
          (
            NES.fromSeqUnsafe(vowelDurs),
            NES.fromSeqUnsafe(consonantDurs),
          ).parMapN(_ + _).toSeq

        // 先頭と末尾にはわずかに無音期間が設定されている。これをSeqの先頭と最後の要素に加算する
        val paddedDurs = durs match {
          case head +: mid :+ last =>
            val headPadding = root.prePhonemeLength.double.getOption(aq).get
            val lastPadding = root.postPhonemeLength.double.getOption(aq).get
            (headPadding + head) +: mid :+ (last + pausesDur.combineAll + lastPadding)
        }

        vowels zip paddedDurs.map { s =>
          val finite = Duration(s"$s second")
          Some(finite).collect { case d: FiniteDuration =>
            d
          }.get
        }
      }

    private lazy val client = {
      import concurrent.duration._
      import scala.language.postfixOps
      EmberClientBuilder
        .default[IO]
        .withTimeout(5 minutes)
        .withIdleConnectionTime(10 minutes)
        .build
    }
  }
}
