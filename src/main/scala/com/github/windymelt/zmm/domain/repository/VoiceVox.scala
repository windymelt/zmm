package com.github.windymelt.zmm.domain.repository

import cats.effect.IO

trait VoiceVoxComponent {
  type AudioQuery
  def voiceVox: VoiceVox

  trait VoiceVox {
    // API
    def audioQuery(text: String, speaker: String): IO[AudioQuery]
    def synthesis(aq: AudioQuery, speaker: String): IO[fs2.Stream[IO, Byte]]
    // misc.
    def controlSpeed(aq: AudioQuery, speed: String): IO[AudioQuery]
  }
}

