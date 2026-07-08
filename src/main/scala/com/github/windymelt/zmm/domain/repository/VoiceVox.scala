package com.github.windymelt.zmm
package domain.repository

import io.circe._
import zio.Task

type AudioQuery = Json
type SpeakerInfo = Json

trait VoiceVox {
  val voiceVoxUri: String
  // API
  def speakers(): Task[SpeakerInfo]
  def audioQuery(text: String, speaker: String): Task[AudioQuery]
  def synthesis(aq: AudioQuery, speaker: String): Task[Array[Byte]]
  // misc.
  def controlSpeed(aq: AudioQuery, speed: String): Task[AudioQuery]
  def registerDict(
      word: String,
      pronounce: String,
      lowerPoint: Int,
  ): Task[Unit]
  def getVowels(aq: AudioQuery): Task[domain.model.VowelSeqWithDuration]
}
