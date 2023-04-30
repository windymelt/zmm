package com.github.windymelt.zmm

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.effect.std.Mutex
import cats.implicits._

class ChromiumCli extends Cli with infrastructure.ChromeScreenShotComponent {
  val chromiumCommand =
    sys.env.get("CHROMIUM_CMD").getOrElse(config.getString("chromium.command"))

  def screenShotResource: IO[Resource[IO, ScreenShot]] = {
    for {
      _ <- logger.debug(
        s"chromium command: $chromiumCommand, chromoumNoSandBox: $chromiumNoSandBox"
      )
      mu <- Mutex[IO]
    } yield mu.lock.map { _ =>
      new ChromeScreenShot(
        chromiumCommand,
        ChromeScreenShot.Quiet,
        chromiumNoSandBox
      )
    }
  }
}
