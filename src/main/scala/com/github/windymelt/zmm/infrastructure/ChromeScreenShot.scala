package com.github.windymelt.zmm
package infrastructure

import cats.effect.IO
import cats.implicits._

trait ChromeScreenShotComponent {
    self: domain.repository.ScreenShotComponent =>

  object ChromeScreenShot {
    sealed trait Verbosity
    final object Quiet extends Verbosity
    final object Verbose extends Verbosity
  }

  type Path = os.Path
  def screenShot: ChromeScreenShot

  class ChromeScreenShot(chromeCommand: String, verbosity: ChromeScreenShot.Verbosity) extends ScreenShot {
    val stdout = verbosity match {
      case ChromeScreenShot.Quiet => os.Pipe
      case ChromeScreenShot.Verbose => os.Inherit
    }
    def takeScreenShot(htmlFilePath: Path, windowWidth: Int = 1920, windowHeight: Int = 1080): IO[Path] = IO.delay {
      os.proc(chromeCommand, "--headless", s"--screenshot=${htmlFilePath}.png", s"--window-size=${windowWidth},${windowHeight}", htmlFilePath)
        .call(stdout = stdout, stderr = stdout, cwd = os.pwd)
    } *> IO.pure(os.Path(s"${htmlFilePath}.png"))
  }
}

