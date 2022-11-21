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

  class ConcreteFFmpeg(verbosity: ConcreteFFmpeg.Verbosity) extends FFmpeg {
    val stdout = verbosity match {
      case ConcreteFFmpeg.Quiet => os.Pipe
      case ConcreteFFmpeg.Verbose => os.Inherit
    }
    def concatenateWavFiles(files: Seq[File]): IO[os.Path] = {
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
          .call(stdout = stdout, stderr = stdout, stdin = fileList, cwd = os.pwd)
      } *>
      IO.pure(os.pwd / os.RelPath("artifacts/concatenated.wav"))
    }

    def getWavDuration(file: File): IO[concurrent.duration.FiniteDuration] = {
      import cats.implicits._
      import scala.util.control.Exception.allCatch
      import concurrent.duration.FiniteDuration

      IO.delay {
        val commandResult = os.proc("ffprobe", (os.pwd / os.RelPath(file))).call(cwd = os.pwd, stderr = os.Pipe, stdout = stdout)
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
            os.proc("ffmpeg", "-protocol_whitelist", "file", "-y", "-f", "concat", "-safe", "0", "-i", "artifacts/cutFile.txt", "-pix_fmt", "yuv420p", "-c:v", "libx264", "artifacts/scenes.mp4")
            .call(stdout = stdout, stderr = stdout, cwd = os.pwd)
        }
        } yield os.pwd / os.RelPath("./artifacts/scenes.mp4")
    }

    def zipVideoWithAudioWithDuration(videoPath: os.Path, audioDurationPair: Seq[(Option[os.Path], FiniteDuration)]): IO[os.Path] = {
      // 一度オーディオをDurationに従って結合し、これと動画を合成する。単に上書きすると元の音声が消えてしまうのでフィルタ合成する。
      val writeCutfile = {
        val cutFileContent = audioDurationPair flatMap { case (pOpt, dur) => pOpt.map(p => s"file ${p}\noutpoint ${dur.toUnit(concurrent.duration.SECONDS)}") } mkString ("\n")
        self.writeStreamToFile(fs2.Stream[IO, Byte](cutFileContent.getBytes():_*), "./artifacts/bgmCutFile.txt")
      }
      for {
        _ <- writeCutfile
        bgm <- IO.delay {
          os.proc("ffmpeg", "-protocol_whitelist", "file", "-y", "-f", "concat", "-safe", "0", "-i", "artifacts/bgmCutFile.txt", "artifacts/concatenatedBGM.wav")
            .call(stdout = stdout, stderr = stdout, cwd = os.pwd)
          os.pwd / os.RelPath("artifacts/concatenatedBGM.wav")
        }
        _ <- IO.delay {
          os.proc("ffmpeg", "-y", "-i", videoPath, "-i", bgm, "-filter_complex", "[0:a][1:a]amerge=inputs=2[a]", "-map", "0:v", "-map", "[a]", "-c:v", "copy", "-ac", "2", "output_with_bgm.mp4")
          .call(stdout = stdout, stderr = stdout, cwd = os.pwd)
        }
      } yield os.pwd / os.RelPath("output_with_bgm.mp4")
    }

    def zipVideoWithAudio(video: os.Path, audio: os.Path): IO[os.Path] = for {
      _ <- IO.delay {
        os.proc("ffmpeg", "-y", "-r", "30", "-i", video, "-i", audio, "-c:v", "copy", "-c:a", "aac", "output.mp4")
        .call(stdout = stdout, stderr = stdout, cwd = os.pwd)
      }
    } yield os.pwd / os.RelPath("output.mp4")
}

}
