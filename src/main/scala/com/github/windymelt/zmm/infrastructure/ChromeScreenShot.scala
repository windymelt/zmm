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

  class ChromeScreenShot(chromeCommand: String, verbosity: ChromeScreenShot.Verbosity, noSandBox: Boolean = false) extends ScreenShot {
    val stdout = verbosity match {
      case ChromeScreenShot.Quiet => os.Pipe
      case ChromeScreenShot.Verbose => os.Inherit
    }
    def takeScreenShot(htmlFilePath: Path, windowWidth: Int = 1920, windowHeight: Int = 1080): IO[Path] = IO.delay {
      val proc = noSandBox match {
        case true =>
          os.proc(chromeCommand, "--headless", "--no-sandbox", s"--screenshot=${htmlFilePath}.png", s"--window-size=${windowWidth},${windowHeight}", htmlFilePath)
        case false =>
          os.proc(chromeCommand, "--headless", s"--screenshot=${htmlFilePath}.png", s"--window-size=${windowWidth},${windowHeight}", htmlFilePath)
      }
      proc.call(stdout = stdout, stderr = stdout, cwd = os.pwd)
    } *> IO.pure(os.Path(s"${htmlFilePath}.png"))
  }
}

