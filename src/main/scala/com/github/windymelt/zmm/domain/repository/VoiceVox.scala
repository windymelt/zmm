package com.github.windymelt.zmm
package domain.repository

import cats.effect.IO
import io.circe._

type AudioQuery = Json
type SpeakerInfo = Json

trait VoiceVox {
  val voiceVoxUri: String
  // API
  def speakers(): IO[SpeakerInfo]
  def audioQuery(text: String, speaker: String): IO[AudioQuery]
  def synthesis(aq: AudioQuery, speaker: String): IO[fs2.Stream[IO, Byte]]
  // misc.
  def controlSpeed(aq: AudioQuery, speed: String): IO[AudioQuery]
  def registerDict(word: String, pronounce: String, lowerPoint: Int): IO[Unit]
  def getVowels(aq: AudioQuery): IO[domain.model.VowelSeqWithDuration]
}
