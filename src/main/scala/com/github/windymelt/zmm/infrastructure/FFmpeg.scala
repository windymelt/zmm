package com.github.windymelt.zmm
package infrastructure

import cats.effect.IO
import scala.concurrent.duration.FiniteDuration

trait FFmpegComponent {
  self: domain.repository.FFmpegComponent with util.UtilComponent =>

  object ConcreteFFmpeg {
    sealed trait Verbosity
    final object Quiet extends Verbosity
    final object Verbose extends Verbosity
  }

  def ffmpeg: ConcreteFFmpeg

    def concatenateWavFiles(files: Seq[File]): IO[File] = {
  class ConcreteFFmpeg(verbosity: ConcreteFFmpeg.Verbosity) extends FFmpeg {
    val stdout = verbosity match {
      case ConcreteFFmpeg.Quiet => os.Pipe
      case ConcreteFFmpeg.Verbose => os.Inherit
    }
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
        commandResult.err.text match {
          case durationRegex(hh, mm, ss, milli) =>
            val toLong = (s: String) => allCatch.opt(s.toLong)
            val to100Long = (s: String) => allCatch.opt((s.toDouble * 10).toLong)

            val hms = Seq(toLong(hh), toLong(mm), toLong(ss), to100Long(milli)).sequence.get
            val units = Seq("hour", "minute", "second", "millisecond")
            hms.zip(units).map(pair => FiniteDuration(pair._1, pair._2)).combineAll
        }
      }
    }

    def concatenateImagesWithDuration(imageDurationPair: Seq[(os.Path, FiniteDuration)]): IO[os.Path] = {
        val writeCutfile = {
          val cutFileContent = imageDurationPair map { case (p, dur) => s"file ${p}\noutpoint ${dur.toUnit(concurrent.duration.SECONDS)}" } mkString ("\n")
          self.writeStreamToFile(fs2.Stream[IO, Byte](cutFileContent.getBytes():_*), "./artifacts/cutFile.txt")
        }

        for {
          _ <- writeCutfile
          _ <- IO.delay {
            // TODO: move to infra layer
            os.proc("ffmpeg", "-protocol_whitelist", "file", "-y", "-f", "concat", "-safe", "0", "-i", "artifacts/cutFile.txt", "artifacts/scenes.avi")
            .call(stdout = os.Inherit, cwd = os.pwd)
        }
        } yield os.pwd / os.RelPath("./artifacts/scenes.avi")
    }
}

}
