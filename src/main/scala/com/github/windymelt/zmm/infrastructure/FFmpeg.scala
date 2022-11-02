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

    def getWavDuration(file: File): IO[concurrent.duration.FiniteDuration] = {
      import cats.implicits._
      import scala.util.control.Exception.allCatch
      import concurrent.duration.FiniteDuration

      IO.println(s"ffprobe ${os.pwd / os.RelPath(file)}") *> IO.delay {
        val commandResult = os.proc("ffprobe", (os.pwd / os.RelPath(file))).call(cwd = os.pwd, stderr = os.Pipe)
        val durationRegex = """Duration: (\d\d):(\d\d):(\d\d)\.(\d\d)""".r.unanchored
        commandResult.err.toString() match {
          case durationRegex(hh, mm, ss, milli) =>
            val toLong = (s: String) => allCatch.opt(s.toLong)
            val to100Long = (s: String) => allCatch.opt((s.toDouble * 10).toLong)

            val hms = Seq(toLong(hh), toLong(mm), toLong(ss), to100Long(milli)).sequence.get
            val units = Seq("hour", "minute", "second", "millisecond")
            hms.zip(units).map(pair => FiniteDuration(pair._1, pair._2)).combineAll
        }
      }
    }
}

}
