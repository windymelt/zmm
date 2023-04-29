package com.github.windymelt.zmm
package infrastructure

import cats.effect.IO
import os.Path

import scala.concurrent.duration.FiniteDuration

trait FFmpegComponent {
  self: domain.repository.FFmpegComponent with util.UtilComponent =>

  object ConcreteFFmpeg {
    sealed trait Verbosity
    final object Quiet extends Verbosity
    final object Verbose extends Verbosity
  }

  def ffmpeg: ConcreteFFmpeg

  class ConcreteFFmpeg(
      ffmpegCommand: String,
      verbosity: ConcreteFFmpeg.Verbosity
  ) extends FFmpeg {
    val FRAME_RATE_FPS = 30 // TODO: application.confなどに逃がす
    val stdout = verbosity match {
      case ConcreteFFmpeg.Quiet   => os.Pipe
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
              ffmpegCommand,
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
              "-ac", // ステレオ化する
              "2",
              "artifacts/concatenated.wav"
            )
            .call(
              stdout = stdout,
              stderr = stdout,
              stdin = fileList,
              cwd = os.pwd
            )
        } *>
        IO.pure(os.pwd / os.RelPath("artifacts/concatenated.wav"))
    }

    def getWavDuration(file: File): IO[concurrent.duration.FiniteDuration] = {
      import cats.implicits._
      import scala.util.control.Exception.allCatch
      import concurrent.duration.FiniteDuration

      IO.delay {
        val commandResult = os
          .proc("ffprobe", (os.pwd / os.RelPath(file)))
          .call(cwd = os.pwd, stderr = os.Pipe, stdout = stdout)
        val durationRegex =
          """Duration: (\d\d):(\d\d):(\d\d)\.(\d\d)""".r.unanchored
        commandResult.err.text match {
          case durationRegex(hh, mm, ss, milli) =>
            val toLong = (s: String) => allCatch.opt(s.toLong)
            val to100Long =
              (s: String) => allCatch.opt((s.toDouble * 10).toLong)

            val hms = Seq(
              toLong(hh),
              toLong(mm),
              toLong(ss),
              to100Long(milli)
            ).sequence.get
            val units = Seq("hour", "minute", "second", "millisecond")
            hms
              .zip(units)
              .map(pair => FiniteDuration(pair._1, pair._2))
              .combineAll
        }
      }
    }

    def concatenateImagesWithDuration(
        imageDurationPair: Seq[(os.Path, FiniteDuration)]
    ): IO[os.Path] = {
      val writeCutfile = {
        val cutFileContent = imageDurationPair map { case (p, dur) =>
          s"file ${p}\noutpoint ${dur.toUnit(concurrent.duration.SECONDS)}"
        } mkString ("\n")
        self.writeStreamToFile(
          fs2.Stream[IO, Byte](cutFileContent.getBytes(): _*),
          "./artifacts/cutFile.txt"
        )
      }
      // スクリーンショット実装がChrome(Chromium)の場合、透明度付きPNGが出力されることを期待して良いので、
      // PNGをレンダした結果と、PNGのアルファチャンネルを取り出した情報とを2つのストリームとしてMKV形式に格納する。
      // MKVは複数ストリームを格納できる動画コンテナなのでこのようなことが可能。
      // スクリーンショット実装がFirefoxの場合、背景を透過したスクリーンショットを撮影する方法がない・・・。

      for {
        _ <- writeCutfile
        _ <- IO.delay {
          // TODO: move to infra layer
          os.proc(
            ffmpegCommand,
            "-protocol_whitelist",
            "file",
            "-y",
            "-f",
            "concat",
            "-safe",
            "0",
            "-i",
            "artifacts/cutFile.txt",
            "-filter_complex",
            // FIXME: 現在Chromiumのバグでサイズがおかしくなっているのでscaleしている
            "split[img][img2];[img2]alphaextract,scale=1920:1080[alpha];[img]scale=1920:1080[scaledimg]",
            "-pix_fmt",
            "yuv420p",
            "-c:v",
            "libx264",
            "-map",
            "[scaledimg]",
            "-map",
            "[alpha]",
            "artifacts/scenes.mkv"
          ).call(stdout = stdout, stderr = stdout, cwd = os.pwd)
        }
      } yield os.pwd / os.RelPath("artifacts/scenes.mkv")
    }

    def zipVideoWithAudioWithDuration(
        videoPath: os.Path,
        audioDurationPair: Seq[(Option[os.Path], FiniteDuration)],
        outputPath: os.Path
    ): IO[os.Path] = {
      // 一度オーディオをDurationに従って結合し、これと動画を合成する。単に上書きすると元の音声が消えてしまうのでフィルタ合成する。
      val writeCutfile = {
        val cutFileContent = audioDurationPair flatMap { case (pOpt, dur) =>
          pOpt.map(p =>
            s"file ${p}\noutpoint ${dur.toUnit(concurrent.duration.SECONDS)}"
          )
        } mkString ("\n")
        self.writeStreamToFile(
          fs2.Stream[IO, Byte](cutFileContent.getBytes(): _*),
          "./artifacts/bgmCutFile.txt"
        )
      }
      for {
        _ <- writeCutfile
        bgm <- IO.delay {
          os.proc(
            ffmpegCommand,
            "-protocol_whitelist",
            "file",
            "-y",
            "-f",
            "concat",
            "-safe",
            "0",
            "-i",
            "artifacts/bgmCutFile.txt",
            "artifacts/concatenatedBGM.wav"
          ).call(stdout = stdout, stderr = stdout, cwd = os.pwd)
          os.pwd / os.RelPath("artifacts/concatenatedBGM.wav")
        }
        _ <- IO.delay {
          os.proc(
            ffmpegCommand,
            "-y",
            "-i",
            videoPath,
            "-i",
            bgm,
            "-filter_complex",
            "[0:a][1:a]amerge=inputs=2[a]",
            "-map",
            "0:v",
            "-map",
            "[a]",
            "-c:v",
            "copy",
            "-ac",
            "2",
            outputPath
          ).call(stdout = stdout, stderr = stdout, cwd = os.pwd)
        }
      } yield outputPath
    }

    /** キャラクターや字幕などの前景の後ろに背景動画を合成し、MP4ファイルを返す。
      *
      * @param overlayVideoPath
      *   前景の動画が収められているMKVファイルのパス。
      * @param baseVideoDurationPair
      *   背景動画と、それが再生されるべき長さ情報のペアによって構成されたSeq。
      * @return
      *   前景と背景を合成したMP4ファイルのパス
      */
    def composeVideoWithDuration(
        overlayVideoPath: os.Path,
        baseVideoDurationPair: Seq[(Option[os.Path], FiniteDuration)]
    ): IO[os.Path] = {
      import cats.implicits._

      // 最初の背景動画が開始するまでの時間を計算する。
      val paddingDur: Double =
        baseVideoDurationPair
          .takeWhile(_._1.isEmpty)
          .map(_._2.toUnit(concurrent.duration.SECONDS))
          .combineAll

      // 背景ビデオが開始する地点まで尺をつなぐためのダミー動画を生成する処理(ffmpegで直接やろうとすると複雑になりすぎる)。
      val genPadding: IO[Option[Path]] =
        if (paddingDur.isEmpty) IO.pure(None)
        else
          IO.delay {
            os.proc(
              ffmpegCommand,
              "-protocol_whitelist",
              "file",
              "-y",
              "-t",
              paddingDur,
              "-filter_complex",
              s"smptehdbars=s=1920x1080:d=$paddingDur, fps=$FRAME_RATE_FPS[v];anullsrc=channel_layout=stereo:sample_rate=24000[o]",
              "-safe",
              "0",
              "-map",
              "[v]",
              "-map",
              "[o]",
              "artifacts/basePadding.mp4"
            ).call(stdout = stdout, stderr = stdout, cwd = os.pwd)
            Some(os.pwd / os.RelPath("artifacts/basePadding.mp4"))
          }

      // 一度背景ビデオをDurationに従って結合する。そのためのcutfileを生成する処理。
      val writeCutfile: Option[os.Path] => IO[os.Path] =
        (pad: Option[os.Path]) => {
          val paddingContent =
            pad.map(p => s"file $p\noutpoint $paddingDur\n").getOrElse("")
          val cutFileContent = baseVideoDurationPair flatMap {
            case (pOpt, dur) =>
              pOpt.map(p =>
                s"file ${p}\noutpoint ${dur.toUnit(concurrent.duration.SECONDS)}"
              )
          } mkString ("\n")
          self.writeStreamToFile(
            fs2.Stream[IO, Byte](
              (paddingContent ++ cutFileContent).getBytes(): _*
            ),
            "./artifacts/baseVideoCutFile.txt"
          ) >> IO.pure(os.Path("./artifacts/baseVideoCutFile.txt", os.pwd))
        }

      // デバッグモードが有効なとき、タイムコードを一緒に合成するためのフィルタ文字列。
      val timecode = verbosity match {
        case ConcreteFFmpeg.Verbose =>
          s";[outv0]drawtext=fontsize=64:box=1:boxcolor=white@0.5:fontcolor=black:fontfile=Berkeley Mono:timecode='00\\:00\\:00\\:00':r=$FRAME_RATE_FPS:y=main_h-text_h:fontcolor=0xccFFFF[outv]"
        case ConcreteFFmpeg.Quiet => ""
      }

      // デバッグ時はタイムコードを合成したビデオストリームを使うための分岐。
      val outputVideoStream = verbosity match {
        case ConcreteFFmpeg.Verbose => "[outv]"
        case ConcreteFFmpeg.Quiet   => "[outv0]"
      }

      // 背景動画の尺は前景動画の尺と合わせる必要があるので、全体の尺をあらかじめ計算しておく。
      val wholeDurationSec: Double = baseVideoDurationPair
        .map(_._2)
        .combineAll
        .toUnit(concurrent.duration.SECONDS)

      // 背景動画を結合するための処理。
      val combineBaseVideo: Path => IO[Path] = (cutFilePath: os.Path) =>
        IO.delay {
          os.proc(
            ffmpegCommand,
            "-protocol_whitelist",
            "file",
            "-y",
            "-f",
            "concat",
            "-safe",
            "0",
            "-i",
            // "artifacts/baseVideoCutFile.txt",
            cutFilePath,
            "-vf",
            s"framerate=$FRAME_RATE_FPS", // こちらは動画なのでfpsではなくframerateフィルタでやや丁寧に処理する
            "artifacts/concatenatedBase.mp4"
          ).call(stdout = stdout, stderr = stdout, cwd = os.pwd)
          os.pwd / os.RelPath("artifacts/concatenatedBase.mp4")
        }

      // 入力されるMKVファイルは第2ストリームにアルファチャンネル情報を格納してある。このアルファチャンネル情報を用いて背景動画に対する合成を行う処理。
      val alphaChannelStreamOverlay = (base: os.Path) =>
        IO.delay {
          os.proc(
            ffmpegCommand,
            "-y",
            "-i",
            overlayVideoPath,
            "-i",
            base,
            "-filter_complex",
            // TODO: overlayVideoPathのFPSが25になっているので30に持ち上げる
            // 必ずbase videoはoverlay video以上の長さである必要があるので、何もない画面にbase videoをoverlayすることで長さを揃えてから再度overlayする
            s"""nullsrc=s=1920x1080:r=$FRAME_RATE_FPS:d=$wholeDurationSec[nullsrc];
                [0:a][1:a]amix=normalize=0[a];
                [nullsrc][1:v]overlay=x=0:y=0[paddedbase];
                [0:0][0:1]alphamerge[overlayv];
                [paddedbase][overlayv]overlay=x=0:y=0:eof_action=pass:shortest=0:repeatlast=1[outv0]${timecode}
                """.stripMargin,
            "-map",
            outputVideoStream,
            "-map",
            "[a]",
            "-shortest",
            "-ac",
            "2",
            "output_composed.mp4"
          ).call(stdout = stdout, stderr = stdout, cwd = os.pwd)
        }
      // TODO: Firefoxを使うときにこちらを起動する
      val colorKeyOverlay = (base: os.Path) =>
        IO.delay {
          os.proc(
            ffmpegCommand,
            "-y",
            "-i",
            overlayVideoPath,
            "-i",
            base,
            "-filter_complex",
            // 必ずbase videoはoverlay video以上の長さである必要があるので、何もない画面にbase videoをoverlayすることで長さを揃えてから再度overlayする
            s"""nullsrc=s=1920x1080:r=$FRAME_RATE_FPS:d=$wholeDurationSec[nullsrc];
                [0:a][1:a]amix=normalize=0[a];
                [nullsrc][1:v]overlay=x=0:y=0[paddedbase];
                [0:v]colorkey=0xFF00FF:0.1:0.5[overlayv];
                [paddedbase][overlayv]overlay=x=0:y=0:eof_action=pass:shortest=0:repeatlast=1[outv0]${timecode}
                """.stripMargin,
            "-map",
            outputVideoStream,
            "-map",
            "[a]",
            "-shortest",
            "-ac",
            "2",
            "output_composed.mp4"
          ).call(stdout = stdout, stderr = stdout, cwd = os.pwd)
        }

      // 定義した処理を合成する。
      for {
        pad <- genPadding
        cutFile <- writeCutfile(pad)
        base <- combineBaseVideo(cutFile)
        // _ <- colorKeyOverlay(base)
        _ <- alphaChannelStreamOverlay(base)
      } yield os.pwd / "output_composed.mp4"
    }

    def zipVideoWithAudio(video: os.Path, audio: os.Path): IO[os.Path] = for {
      _ <- IO.delay {
        os.proc(
          ffmpegCommand,
          "-y",
          "-r",
          "30",
          "-i",
          video,
          "-i",
          audio,
          "-c:v",
          "copy",
          "-c:a",
          "aac",
          "-map",
          "0:0",
          "-map",
          "0:1",
          "-map",
          "1:a",
          "output.mkv"
        ).call(stdout = stdout, stderr = stdout, cwd = os.pwd)
      }
    } yield os.pwd / "output.mkv"

    def generateSilentWav(path: os.Path, length: FiniteDuration): IO[os.Path] =
      for {
        _ <- IO.delay {
          val sample = 24000 // VOICEVOXに揃えないと伸びてしまう
          val lengthSec = length.toSeconds
          os.proc(
            ffmpegCommand,
            "-y",
            "-t",
            lengthSec,
            "-f",
            "lavfi",
            "-i",
            s"anullsrc=cl=mono:r=${sample}",
            "-sample_fmt",
            "s16", // depth 16
            path
          ).call(stdout = stdout, stderr = stdout, cwd = os.pwd)
        }
      } yield path
  }
}
