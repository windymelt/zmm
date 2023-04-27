package com.github.windymelt.zmm.domain.repository

import cats.effect.IO

trait FFmpegComponent {

  def ffmpeg: FFmpeg

  trait FFmpeg {
    type File = String // TODO: 必要に応じて拡張する
    import concurrent.duration.FiniteDuration
    def concatenateWavFiles(files: Seq[File]): IO[os.Path]
    def getWavDuration(file: File): IO[FiniteDuration]
    def concatenateImagesWithDuration(
        imageDurationPair: Seq[(os.Path, FiniteDuration)]
    ): IO[os.Path]
    def zipVideoWithAudioWithDuration(
        videoPath: os.Path,
        audioDurationPair: Seq[(Option[os.Path], FiniteDuration)]
    ): IO[os.Path]
    def composeVideoWithDuration(
        baseVideoPath: os.Path,
        overlayVideoDurationPair: Seq[(Option[os.Path], FiniteDuration)]
    ): IO[os.Path]
    def zipVideoWithAudio(videoPath: os.Path, audioPath: os.Path): IO[os.Path]
    def generateSilentWav(path: os.Path, length: FiniteDuration): IO[os.Path]
  }
}
