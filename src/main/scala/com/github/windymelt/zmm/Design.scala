package com.github.windymelt.zmm

import com.typesafe.config.Config
import sttp.client4.httpclient.zio.HttpClientZioBackend
import zio.Semaphore
import zio.ZIO
import zio.ZLayer
import infrastructure.{ChromeScreenShot, ConcreteFFmpeg, FirefoxScreenShot}

object Design:
  def chrome(
      config: Config,
      logLevel: String = "INFO",
  ): ZLayer[Any, Throwable, Cli] = {
    val chromiumCommand =
      sys.env
        .get("CHROMIUM_CMD").getOrElse(config.getString("chromium.command"))

    val chromiumNoSandBox = sys.env
      .get("CHROMIUM_NOSANDBOX")
      .map(_ == "1")
      .getOrElse(config.getBoolean("chromium.nosandbox"))

    cliLayer(config, logLevel) { () =>
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
  ): ZLayer[Any, Throwable, Cli] = {
    val firefoxCommand =
      sys.env.get("FIREFOX_CMD").getOrElse(config.getString("firefox.command"))

    cliLayer(config, logLevel) { () =>
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

  private def cliLayer(config: Config, logLevel: String)(
      makeScreenShot: () => domain.repository.ScreenShot,
  )(logSetting: zio.UIO[Unit]): ZLayer[Any, Throwable, Cli] = {
    val ffmpegVerbosity = logLevel match
      case "DEBUG" => ConcreteFFmpeg.Verbose
      case "TRACE" => ConcreteFFmpeg.Verbose
      case _       => ConcreteFFmpeg.Quiet

    val voiceVoxUri =
      sys.env.getOrElse("VOICEVOX_URI", config.getString("voicevox.apiUri"))

    ZLayer.scoped {
      for {
        _ <- logSetting
        backend <- HttpClientZioBackend.scoped()
        // スクリーンショットバックエンドは同時起動できないためSemaphore(1)で直列化する
        sem <- Semaphore.make(1)
      } yield new Cli(
        voiceVox = infrastructure.ConcreteVoiceVox(voiceVoxUri, backend),
        ffmpeg = ConcreteFFmpeg("ffmpeg", ffmpegVerbosity),
        screenShot = ScreenShotService(sem, makeScreenShot),
      )
    }
  }
