package com.github.windymelt.zmm.domain.model

sealed trait VoiceBackendConfig
final case class VoiceVoxBackendConfig(speakerId: String)
    extends VoiceBackendConfig

final case class CharacterConfig(name: String, voiceId: String)

final case class Context(
    voiceConfigMap: Map[String, VoiceBackendConfig] = Map.empty,
    characterConfigMap: Map[String, CharacterConfig] = Map.empty,
    backgroundImageUrl: Option[String] = None,
    spokenByCharacterId: Option[String] = None,
    speed: Option[String] = Some("1.0"),
    // TODO: BGM, fontColor, etc.
)

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
    def combine(x: Context, y: Context): Context = Context(
      voiceConfigMap = x.voiceConfigMap ++ y.voiceConfigMap,
      characterConfigMap = x.characterConfigMap ++ y.characterConfigMap,
      backgroundImageUrl =
        y.backgroundImageUrl orElse x.backgroundImageUrl, // 後勝ち
      spokenByCharacterId = y.spokenByCharacterId orElse x.spokenByCharacterId, // 後勝ち
      speed = y.speed orElse x.speed // 後勝ち
    )
    def empty: Context = Context.empty
  }

  def fromNode(
      dialogueElem: scala.xml.Node,
      currentContext: Context = Context.empty
  ): Seq[(Say, Context)] = dialogueElem match {
    case Comment(_) => Seq.empty // コメントは無視する
    case Text(t) if t.forall(_.isWhitespace) => Seq.empty // 空行やただの入れ子でコンテキストが生成されないようにする
    case Text(t) => Seq(Say(t) -> currentContext)
    case e: Elem =>
      e.child.flatMap(c => fromNode(c, currentContext |+| extract(e)))
  }

  private def firstAttrTextOf(e: Elem, a: String): Option[String] = e.attribute(a).headOption.flatMap(_.headOption).map(_.text)

  private def extract(e: Elem): Context = {
    Context(
      voiceConfigMap = empty.voiceConfigMap, // TODO
      characterConfigMap = empty.characterConfigMap, // TODO
      backgroundImageUrl = firstAttrTextOf(e, "backgroundImage"),
      spokenByCharacterId = firstAttrTextOf(e, "by"),
      speed = firstAttrTextOf(e, "speed"),
    )
  }

  val empty: Context = Context()
}
