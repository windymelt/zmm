package com.github.windymelt.zmm

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.effect.std.Mutex

class FirefoxCli(logLevel: String = "INFO")
    extends Cli(logLevel = logLevel)
    with infrastructure.FirefoxScreenShotComponent {
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
        }
      )
    }
}
