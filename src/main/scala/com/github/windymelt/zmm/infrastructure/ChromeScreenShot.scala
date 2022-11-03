package com.github.windymelt.zmm
package infrastructure

import cats.effect.IO
import cats.implicits._

trait ChromeScreenShotComponent {
    self: domain.repository.ScreenShotComponent =>

  type Path = os.Path
  def screenShot: ChromeScreenShot

  class ChromeScreenShot(chromeCommand: String) extends ScreenShot {
    def takeScreenShot(htmlFilePath: Path, windowWidth: Int = 1920, windowHeight: Int = 1080): IO[Path] = IO.delay {
      os.proc(chromeCommand, "--headless", s"--screenshot=${htmlFilePath}.png", s"--window-size=${windowWidth},${windowHeight}", htmlFilePath)
        .call(stdout = os.Inherit, cwd = os.pwd)
    } *> IO.pure(os.Path(s"${htmlFilePath}.png"))
  }
}

