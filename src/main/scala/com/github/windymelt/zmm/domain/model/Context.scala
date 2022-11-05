package com.github.windymelt.zmm.domain.model

sealed trait VoiceBackendConfig
final case class VoiceVoxBackendConfig(speakerId: String)
    extends VoiceBackendConfig

final case class CharacterConfig(name: String, voiceId: String)

final case class Context(
  voiceConfigMap: Map[String, VoiceBackendConfig],
  characterConfigMap: Map[String, CharacterConfig],
  backgroundImageUrl: Option[String],
)

