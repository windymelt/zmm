package com.github.windymelt.zmm
package infrastructure

import cats.effect.IO

trait FFmpegComponent {
  self: domain.repository.FFmpegComponent =>

  def ffmpeg: ConcreteFFmpeg

  class ConcreteFFmpeg extends FFmpeg {
    def concatenateWavFiles(files: Seq[File]): IO[File] = {
      // stub
      val fileList = files.map(f => s"file '${f}'").mkString("\n")
      val fileListPath = os.pwd / "fileList.txt"
      IO.delay {
        os.remove(fileListPath, checkExists = false)
        os.write(fileListPath, fileList)
      } *>
      IO.delay {
        os
          .proc(
            "ffmpeg",
            "-protocol_whitelist",
            "file",
            "-y", // overwrite if exists
            "-f",
            "concat",
            "-safe", // go despite of not being absolute path
            "0",
            "-i",
            "fileList.txt",
            "-c",
            "copy",
            "artifacts/concatenated.wav"
          )
          .call(stdout = os.Inherit, stdin = fileList, cwd = os.pwd)
      } *>
      IO.pure("artifacts/concatenated.wav")
    }
  }
}
