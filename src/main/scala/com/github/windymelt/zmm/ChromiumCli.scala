package com.github.windymelt.zmm

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.effect.std.Semaphore
import cats.implicits._

class ChromiumCli extends Cli with infrastructure.ChromeScreenShotComponent {
  val chromiumCommand =
    sys.env.get("CHROMIUM_CMD").getOrElse(config.getString("chromium.command"))

  def screenShotResource: IO[Resource[IO, ScreenShot]] = {
    for {
      _ <- IO.println(
        s"""[configuration] chromium command: ${chromiumCommand}"""
      )
      smph <- Semaphore[IO](4) // TODO: go configuration
    } yield smph.permit.map { _ =>
      new ChromeScreenShot(
        chromiumCommand,
        ChromeScreenShot.Quiet,
        chromiumNoSandBox
      )
    }
  }
}
