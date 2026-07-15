package com.github.windymelt.zmm

import com.typesafe.config.Config
import sttp.client4.httpclient.zio.HttpClientZioBackend
import zio.Semaphore
import zio.Task
import zio.ZIO
import zio.ZLayer
import infrastructure.{
  ChromeScreenShot,
  ConcreteFFmpeg,
  DockerFFmpeg,
  FirefoxScreenShot,
}

object Design:
  def chrome(
      config: Config,
      logLevel: String = "INFO",
      ffmpegBackend: FFmpegBackend = FFmpegBackend.Local,
  ): ZLayer[Any, Throwable, Cli] = {
    val chromiumCommand =
      sys.env
        .get("CHROMIUM_CMD").getOrElse(config.getString("chromium.command"))

    val chromiumNoSandBox = sys.env
      .get("CHROMIUM_NOSANDBOX")
      .map(_ == "1")
      .getOrElse(config.getBoolean("chromium.nosandbox"))

    cliLayer(config, logLevel, ffmpegBackend) { () =>
      new ChromeScreenShot(
        chromiumCommand,
        logLevel match {
          case "TRACE" => ChromeScreenShot.Verbose
          case "DEBUG" => ChromeScreenShot.Verbose
          case _       => ChromeScreenShot.Quiet
        },
        chromiumNoSandBox,
      )
    }(
      ZIO.logDebug(
        s"chromium command: $chromiumCommand, chromoumNoSandBox: $chromiumNoSandBox",
      ),
    )
  }

  def firefox(
      config: Config,
      logLevel: String = "INFO",
      ffmpegBackend: FFmpegBackend = FFmpegBackend.Local,
  ): ZLayer[Any, Throwable, Cli] = {
    val firefoxCommand =
      sys.env.get("FIREFOX_CMD").getOrElse(config.getString("firefox.command"))

    cliLayer(config, logLevel, ffmpegBackend) { () =>
      new FirefoxScreenShot(
        firefoxCommand,
        logLevel match {
          case "TRACE" => FirefoxScreenShot.Verbose
          case "DEBUG" => FirefoxScreenShot.Verbose
          case _       => FirefoxScreenShot.Quiet
        },
      )
    }(ZIO.logDebug(s"firefox command: $firefoxCommand"))

  }

  private def cliLayer(
      config: Config,
      logLevel: String,
      ffmpegBackend: FFmpegBackend,
  )(
      makeScreenShot: () => domain.repository.ScreenShot,
  )(logSetting: zio.UIO[Unit]): ZLayer[Any, Throwable, Cli] = {
    val ffmpegVerbosity = logLevel match
      case "DEBUG" => ConcreteFFmpeg.Verbose
      case "TRACE" => ConcreteFFmpeg.Verbose
      case _       => ConcreteFFmpeg.Quiet

    val voiceVoxUri =
      sys.env.getOrElse("VOICEVOX_URI", config.getString("voicevox.apiUri"))

    // ffmpegバックエンドの組み立て。Dockerの場合はLayer構築時にイメージをビルドする
    val makeFFmpeg: Task[domain.repository.FFmpeg] = ffmpegBackend match
      case FFmpegBackend.Local =>
        ZIO.succeed(
          ConcreteFFmpeg(config.getString("ffmpeg.command"), ffmpegVerbosity),
        )
      case FFmpegBackend.Docker =>
        val imageTag = sys.env.getOrElse(
          "FFMPEG_DOCKER_IMAGE",
          config.getString("ffmpeg.dockerImage"),
        )
        ZIO.logInfo(s"Building ffmpeg docker image: $imageTag") *>
          DockerFFmpeg
            .buildImage(imageTag, ffmpegVerbosity)
            .as(DockerFFmpeg(imageTag, ffmpegVerbosity))

    ZLayer.scoped {
      for {
        _ <- logSetting
        backend <- HttpClientZioBackend.scoped()
        ffmpeg <- makeFFmpeg
        // スクリーンショットバックエンドは同時起動できないためSemaphore(1)で直列化する
        sem <- Semaphore.make(1)
      } yield new Cli(
        voiceVox = infrastructure.ConcreteVoiceVox(voiceVoxUri, backend),
        ffmpeg = ffmpeg,
        screenShot = ScreenShotService(sem, makeScreenShot),
      )
    }
  }
