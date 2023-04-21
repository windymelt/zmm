package com.github.windymelt.zmm

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.implicits._

class ChromiumCli extends Cli with infrastructure.ChromeScreenShotComponent {
  val chromiumCommand =
    sys.env.get("CHROMIUM_CMD").getOrElse(config.getString("chromium.command"))

  def screenShotResource: IO[Resource[IO, ScreenShot]] = {
    IO.println(
      s"""[configuration] chromium command: ${chromiumCommand}"""
    ) >> Resource
      .eval(
        new ChromeScreenShot(
          chromiumCommand,
          ChromeScreenShot.Quiet,
          chromiumNoSandBox
        ).pure[IO]
      )
      .pure[IO]
  }

}
