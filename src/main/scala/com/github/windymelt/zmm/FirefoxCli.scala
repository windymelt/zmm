package com.github.windymelt.zmm

import cats.effect.IO
import cats.effect.std.Mutex
import cats.effect.kernel.Resource

class FirefoxCli extends Cli with infrastructure.FirefoxScreenShotComponent {
  val firefoxCommand =
    sys.env.get("FIREFOX_CMD").getOrElse(config.getString("firefox.command"))

  def screenShotResource: IO[Resource[IO, ScreenShot]] =
    for {
      _ <- IO.println(
        s"""[configuration] firefox command: ${firefoxCommand}"""
      )
      mx <- Mutex[IO]
    } yield mx.lock.map { _ =>
      new FirefoxScreenShot(firefoxCommand, FirefoxScreenShot.Quiet)
    }
}
