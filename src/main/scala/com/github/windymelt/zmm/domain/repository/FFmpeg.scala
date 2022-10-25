package com.github.windymelt.zmm.domain.repository

import cats.effect.IO

trait FFmpegComponent {

  def ffmpeg: FFmpeg

  trait FFmpeg {
    type File = String // TODO: 必要に応じて拡張する
    def concatenateWavFiles(files: Seq[File]): IO[File]
  }
}
