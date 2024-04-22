package com.github.windymelt.zmm

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.effect.std.Mutex
import com.github.windymelt.zmm.domain.repository.{FFmpeg, ScreenShot, VoiceVox}
import com.typesafe.config.Config
import wvlet.airframe.*
import infrastructure.{ChromeScreenShot, ConcreteFFmpeg, FirefoxScreenShot}
import org.typelevel.log4cats.Logger

object Design:
  def chrome(config: Config, logLevel: String = "INFO", logger: Logger[IO]) = {
    val ffmpegCommand = config.getString("ffmpeg.command")
    val ffmpegVerbosity = logLevel match
      case "DEBUG" => ConcreteFFmpeg.Verbose
      case "TRACE" => ConcreteFFmpeg.Verbose
      case _       => ConcreteFFmpeg.Quiet

    val voiceVoxUri =
      sys.env.getOrElse("VOICEVOX_URI", config.getString("voicevox.apiUri"))

    val chromiumCommand =
      sys.env
        .get("CHROMIUM_CMD").getOrElse(config.getString("chromium.command"))

    val chromiumNoSandBox = sys.env
      .get("CHROMIUM_NOSANDBOX")
      .map(_ == "1")
      .getOrElse(config.getBoolean("chromium.nosandbox"))

    def screenShotResource: IO[Resource[IO, ScreenShot]] = {
      for {
        _ <- logger.debug(
          s"chromium command: $chromiumCommand, chromoumNoSandBox: $chromiumNoSandBox",
        )
        mu <- Mutex[IO]
      } yield mu.lock.map { _ =>
        new infrastructure.ChromeScreenShot(
          chromiumCommand,
          logLevel match {
            case "TRACE" => ChromeScreenShot.Verbose
            case "DEBUG" => ChromeScreenShot.Verbose
            case _       => ChromeScreenShot.Quiet
          },
          chromiumNoSandBox,
        )
      }
    }

    newDesign
      .bind[FFmpeg].toInstance(
        ConcreteFFmpeg("ffmpeg", ffmpegVerbosity),
      )
      .bind[IO[Resource[IO, ScreenShot]]].toInstance(screenShotResource)
      .bind[VoiceVox].toInstance(
        infrastructure.ConcreteVoiceVox(voiceVoxUri, logger),
      )
  }

  def firefox(config: Config, logLevel: String = "INFO", logger: Logger[IO]) = {
    val ffmpegCommand = config.getString("ffmpeg.command")
    val ffmpegVerbosity = logLevel match
      case "DEBUG" => ConcreteFFmpeg.Verbose
      case "TRACE" => ConcreteFFmpeg.Verbose
      case _       => ConcreteFFmpeg.Quiet

    val voiceVoxUri =
      sys.env.getOrElse("VOICEVOX_URI", config.getString("voicevox.apiUri"))

    val firefoxCommand =
      sys.env.get("FIREFOX_CMD").getOrElse(config.getString("firefox.command"))

    def screenShotResource: IO[Resource[IO, ScreenShot]] =
      for {
        _ <- logger.debug(s"firefox command: $firefoxCommand")
        mx <- Mutex[IO]
      } yield mx.lock.map { _ =>
        new FirefoxScreenShot(
          firefoxCommand,
          logLevel match {
            case "TRACE" => FirefoxScreenShot.Verbose
            case "DEBUG" => FirefoxScreenShot.Verbose
            case _       => FirefoxScreenShot.Quiet
          },
        )
      }

    newDesign
      .bind[FFmpeg].toInstance(
        ConcreteFFmpeg("ffmpeg", ffmpegVerbosity),
      )
      .bind[IO[Resource[IO, ScreenShot]]].toInstance(screenShotResource)
      .bind[VoiceVox].toInstance(
        infrastructure.ConcreteVoiceVox(voiceVoxUri, logger),
      )
  }

  extension (d: Design)
    // Because `build` is inline function, we should define `runIO` as inline function.
    /** Wraps Session from Airframe DI with Resource.
      */
    inline def runIO[A, B](f: A => IO[B]): IO[B] =
      import scala.util.chaining.*
      Resource
        .make(IO(d.newSession.tap(_.start)))(s => IO(s.shutdown))
        .map(_.build[A])
        .use(f)
