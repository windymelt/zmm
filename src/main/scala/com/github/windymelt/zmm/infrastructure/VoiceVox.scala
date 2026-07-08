package com.github.windymelt.zmm
package infrastructure

import io.circe._
import sttp.client4._
import sttp.client4.circe._
import zio.Task
import zio.ZIO

import concurrent.duration._
import scala.language.postfixOps

type AudioQuery = Json // TODO: 必要に応じて高級なcase class / HListにする
type SpeakerInfo = Json // TODO: 必要に応じて高級なcase class / HListにする

/** VOICEVOX client.
  *
  * You can start local VOICEVOX container with Docker:
  * {{{
  * docker run --rm -it -p '127.0.0.1:50021:50021' voicevox/voicevox_engine:cpu-ubuntu20.04-latest
  * }}}
  * These code can call `ZIO.fail()`.
  */
class ConcreteVoiceVox(
    val voiceVoxUri: String,
    backend: Backend[Task],
) extends domain.repository.VoiceVox {
  // VOICEVOXは合成に時間がかかることがあるのでタイムアウトを長めにとっておく
  private val requestTimeout = 5 minutes

  def speakers(): Task[SpeakerInfo] = for {
    _ <- ZIO.logDebug(s"Requesting ${voiceVoxUri}/speakers")
    res <- basicRequest
      .get(uri"${voiceVoxUri}/speakers")
      .readTimeout(requestTimeout)
      .response(asJson[SpeakerInfo])
      .send(backend)
    body <- ZIO.fromEither(res.body)
  } yield body

  def audioQuery(text: String, speaker: String): Task[AudioQuery] = for {
    _ <- ZIO.logDebug(s"Requesting ${voiceVoxUri}/audio_query")
    res <- basicRequest
      .post(uri"${voiceVoxUri}/audio_query?speaker=$speaker&text=$text")
      .header("accept", "application/json")
      .readTimeout(requestTimeout)
      .response(asJson[AudioQuery])
      .send(backend)
    body <- ZIO.fromEither(res.body)
  } yield body

  def synthesis(aq: AudioQuery, speaker: String): Task[Array[Byte]] = for {
    _ <- ZIO.logDebug(s"Requesting ${voiceVoxUri}/synthesis")
    res <- basicRequest
      .post(uri"${voiceVoxUri}/synthesis?speaker=$speaker")
      .header("Content-Type", "application/json")
      .body(aq.noSpaces)
      .readTimeout(requestTimeout)
      .response(asByteArray)
      .send(backend)
    body <- ZIO.fromEither(res.body).mapError(msg => new Exception(msg))
  } yield body

  def controlSpeed(aq: AudioQuery, speed: String): Task[AudioQuery] =
    import io.circe.syntax._
    ZIO
      .fromOption(
        aq.hcursor.downField("speedScale").withFocus(_ => speed.asJson).top,
      )
      .orElseFail(Exception("speedScale not found"))

  def registerDict(
      word: String,
      pronounce: String,
      lowerPoint: Int,
  ): Task[Unit] = for {
    _ <- ZIO.logDebug(s"Requesting ${voiceVoxUri}/user_dict_word")
    res <- basicRequest
      .post(
        uri"${voiceVoxUri}/user_dict_word?surface=$word&pronunciation=$pronounce&accent_type=${lowerPoint.toString}",
      )
      .header("Content-Type", "application/json")
      .readTimeout(requestTimeout)
      .send(backend)
    _ <- ZIO
      .fail(Exception(s"Failed to register dict: ${res.code}"))
      .unless(res.code.isSuccess)
  } yield ()

  def getVowels(aq: AudioQuery): Task[domain.model.VowelSeqWithDuration] =
    ZIO.attempt {
      import io.circe.optics.JsonPath._
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
          .map:
            case d if d.isNaN => 0.0
            case d            => d

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
      val paddedDurs = durs match
        case head +: mid :+ last =>
          val headPadding = root.prePhonemeLength.double.getOption(aq).get
          val lastPadding = root.postPhonemeLength.double.getOption(aq).get
          (headPadding + head) +: mid :+ (last + pausesDur.combineAll + lastPadding)

      vowels zip paddedDurs.map(_ seconds)
    }
}
