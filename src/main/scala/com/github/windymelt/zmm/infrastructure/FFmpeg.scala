package com.github.windymelt.zmm
  package infrastructure

import cats.effect.IO

trait FFmpegComponent {
  self: domain.repository.FFmpegComponent =>

  def ffmpeg: ConcreteFFmpeg

  class ConcreteFFmpeg extends FFmpeg {
    def concatenateWavFiles(files: Seq[File]): IO[File] = {
      // stub
      val proc = os.proc("ffmpeg", "-version").call(stdout = os.Inherit)

      IO.pure("concatenated.wav")
    }
  }
}

