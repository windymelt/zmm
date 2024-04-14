package com.github.windymelt.zmm

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.effect.std.Mutex
import cats.implicits._

class ChromiumCli(logLevel: String = "INFO")
    extends Cli(logLevel = logLevel)
    with infrastructure.ChromeScreenShotComponent {
  val chromiumCommand =
    sys.env.get("CHROMIUM_CMD").getOrElse(config.getString("chromium.command"))

  def screenShotResource: IO[Resource[IO, ScreenShot]] = {
    for {
      _ <- logger.debug(
        s"chromium command: $chromiumCommand, chromoumNoSandBox: $chromiumNoSandBox",
      )
      mu <- Mutex[IO]
    } yield mu.lock.map { _ =>
      new ChromeScreenShot(
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
}
