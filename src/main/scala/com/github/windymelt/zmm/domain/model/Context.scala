package com.github.windymelt.zmm.domain.model

import scala.concurrent.duration.FiniteDuration

sealed trait VoiceBackendConfig
final case class VoiceVoxBackendConfig(speakerId: String)
    extends VoiceBackendConfig
final case class SilentBackendConfig() extends VoiceBackendConfig

final case class CharacterConfig(
    name: String,
    voiceId: String,
    serifColor: Option[String] = None,
    tachieUrl: Option[String] = None // セリフカラー同様、セリフによって上書きされうる
)

/*
 Contextの4要素:
 - Context class
 - Contextを合成するためのMonoid instance
 - Elem -> Contextする写像
 - 初期値をroot Elem -> Contextで決定する写像
 これがフィールドごとに一列になっていると便利なのだけれど動的にクラスを組み立てることは(Shapelessなどを使わないかぎり)不可能なので、
 下3つを直列記述するbuilderを作ることを今後のTODOとしたい
 */

final case class Context(
    voiceConfigMap: Map[String, VoiceBackendConfig] = Map.empty,
    characterConfigMap: Map[String, CharacterConfig] = Map.empty,
    backgroundImageUrl: Option[String] = None,
    spokenByCharacterId: Option[String] = None,
    spokenVowels: Option[Seq[(String, FiniteDuration)]] = None, // 口パクのために使う母音情報
    speed: Option[String] = Some("1.0"),
    font: Option[String] = None,
    serifColor: Option[String] = None, // どう使うかはテンプレート依存
    tachieUrl: Option[String] = None,
    dict: Seq[(String, String, Int)] = Seq.empty,
    additionalTemplateVariables: Map[String, String] = Map.empty,
    bgm: Option[String] = None,
    codes: Map[String, (String, Option[String])] =
      Map.empty, // id -> (code, lang?)
    maths: Map[String, String] = Map.empty, // id -> LaTeX string
    sic: Option[String] = None, // 代替読みを設定できる(数式などで使う)
    silentLength: Option[FiniteDuration] = None, // by=silentな場合に停止する時間
    video: Option[String] = None // 背景に合成する動画
    // TODO: BGM, fontColor, etc.
) {
  def atv = additionalTemplateVariables // alias for template
}

// TODO: 後で動かす
sealed trait DialogueTree
final case class Say(val text: String)
// TODO: 仮にsceneだけとしている(他にも色々ありそう)
final case class Scene(children: Seq[DialogueTree])

object Context {
  import cats._
  import cats.implicits._
  import scala.xml.{Text, Node, Elem, Comment}

  // Context is a Monoid
  implicit val monoidForContext = new Monoid[Context] {
    def combine(x: Context, y: Context): Context = {
      val spokenByCharacterId = y.spokenByCharacterId |+| x.spokenByCharacterId
      val characterConfigMap = x.characterConfigMap ++ y.characterConfigMap
      val serifColor =
        y.serifColor orElse x.serifColor orElse spokenByCharacterId
          .flatMap(characterConfigMap.get)
          .flatMap(_.serifColor)
      val tachieUrl = y.tachieUrl orElse x.tachieUrl orElse spokenByCharacterId
        .flatMap(characterConfigMap.get)
        .flatMap(_.tachieUrl)
      Context(
        voiceConfigMap = x.voiceConfigMap ++ y.voiceConfigMap,
        characterConfigMap = characterConfigMap,
        backgroundImageUrl =
          y.backgroundImageUrl orElse x.backgroundImageUrl, // 後勝ち
        spokenByCharacterId = spokenByCharacterId,
        spokenVowels = y.spokenVowels <+> x.spokenVowels,
        speed = y.speed orElse x.speed, // 後勝ち
        font = y.font orElse x.font, // 後勝ち
        serifColor = serifColor,
        tachieUrl = tachieUrl,
        dict = y.dict |+| x.dict,
        additionalTemplateVariables =
          x.additionalTemplateVariables ++ y.additionalTemplateVariables,
        bgm = y.bgm orElse x.bgm,
        codes =
          x.codes |+| y.codes, // Map の Monoid性を応用すると、同一idで書かれたコードは結合されるという好ましい特性が表われるのでこうしている。additionalTemplateVariablesに畳んでもいいかもしれない。現在のコードはadditionalTemplateVariablesに入れている
        maths = x.maths |+| y.maths,
        sic = y.sic orElse x.sic,
        silentLength = y.silentLength <+> x.silentLength,
        video = y.video <+> x.video
      )
    }
    def empty: Context = Context.empty
  }

  def fromNode(
      dialogueElem: scala.xml.Node,
      currentContext: Context = Context.empty
  ): Seq[(Say, Context)] = dialogueElem match {
    case Comment(_) => Seq.empty // コメントは無視する
    case Text(t) if t.forall(_.isWhitespace) =>
      Seq.empty // 空行やただの入れ子でコンテキストが生成されないようにする
    case Text(t) => Seq(Say(t) -> currentContext)
    case e: Elem =>
      e.child.flatMap(c => fromNode(c, currentContext |+| extract(e)))
  }

  private def firstAttrTextOf(e: Elem, a: String): Option[String] =
    e.attribute(a).headOption.flatMap(_.headOption).map(_.text)

  private def extract(e: Elem): Context = {
    val atvs = {
      val motif = firstAttrTextOf(e, "motif").map("motif" -> _)
      val code = firstAttrTextOf(e, "code").map("code" -> _)
      val math = firstAttrTextOf(e, "math").map("math" -> _)
      Seq(motif, code, math).flatten.toMap
    }
    Context(
      voiceConfigMap = empty.voiceConfigMap, // TODO
      characterConfigMap = empty.characterConfigMap, // TODO
      backgroundImageUrl =
        firstAttrTextOf(e, "backgroundImage"), // TODO: no camelCase
      spokenByCharacterId = firstAttrTextOf(e, "by"),
      speed = firstAttrTextOf(e, "speed"),
      font = firstAttrTextOf(e, "font"),
      serifColor = firstAttrTextOf(e, "serif-color"),
      tachieUrl = firstAttrTextOf(e, "tachie-url"),
      additionalTemplateVariables = atvs,
      bgm = firstAttrTextOf(e, "bgm"),
      sic = firstAttrTextOf(e, "sic"),
      silentLength = firstAttrTextOf(e, "silent-length").map(l =>
        FiniteDuration.apply(Integer.parseInt(l), "second")
      ),
      video = firstAttrTextOf(e, "video")
    )
  }

  val empty: Context = Context()
}
