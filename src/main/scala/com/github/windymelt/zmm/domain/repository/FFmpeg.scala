package com.github.windymelt.zmm.domain.repository

import zio.Task

trait FFmpeg {
  type File = String // TODO: 必要に応じて拡張する
  import concurrent.duration.FiniteDuration

  val ffmpegCommand: String
  def concatenateWavFiles(files: Seq[File]): Task[os.Path]
  def getWavDuration(file: File): Task[FiniteDuration]
  def concatenateImagesWithDuration(
      imageDurationPair: Seq[(os.Path, FiniteDuration)],
  ): Task[os.Path]
  def zipVideoWithAudioWithDuration(
      videoPath: os.Path,
      audioDurationPair: Seq[(Option[os.Path], FiniteDuration)],
      outputPath: os.Path,
  ): Task[os.Path]
  def composeVideoWithDuration(
      baseVideoPath: os.Path,
      overlayVideoDurationPair: Seq[(Option[os.Path], FiniteDuration)],
  ): Task[os.Path]
  def zipVideoWithAudio(videoPath: os.Path, audioPath: os.Path): Task[os.Path]
  def generateSilentWav(path: os.Path, length: FiniteDuration): Task[os.Path]
}
