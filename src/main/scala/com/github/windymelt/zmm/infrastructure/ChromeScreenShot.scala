package com.github.windymelt.zmm
package infrastructure

import cats.effect.IO
import cats.implicits._
import cats.effect.kernel.Resource

trait ChromeScreenShotComponent {
  self: domain.repository.ScreenShotComponent =>

  object ChromeScreenShot {
    sealed trait Verbosity
    final object Quiet extends Verbosity
    final object Verbose extends Verbosity
  }

  def screenShotResource: IO[Resource[IO, ScreenShot]]

  class ChromeScreenShot(
      chromeCommand: String,
      verbosity: ChromeScreenShot.Verbosity,
      noSandBox: Boolean = false
  ) extends ScreenShot {
    val stdout = verbosity match {
      case ChromeScreenShot.Quiet   => os.Pipe
      case ChromeScreenShot.Verbose => os.Inherit
    }
    def takeScreenShot(
        htmlFilePath: os.Path,
        windowWidth: Int = 1920,
        windowHeight: Int = 1080
    ): IO[os.Path] = IO.delay {
      val proc = noSandBox match {
        case true =>
          os.proc(
            chromeCommand,
            "--headless",
            "--no-sandbox",
            s"--screenshot=${htmlFilePath}.png",
            s"--window-size=${windowWidth},${windowHeight}",
            "--default-background-color=00000000",
            htmlFilePath
          )
        case false =>
          os.proc(
            chromeCommand,
            "--headless",
            s"--screenshot=${htmlFilePath}.png",
            s"--window-size=${windowWidth},${windowHeight}",
            "--default-background-color=00000000",
            htmlFilePath
          )
      }
      proc.call(stdout = stdout, stderr = stdout, cwd = os.pwd)
    } *> IO.pure(os.Path(s"${htmlFilePath}.png"))
  }
}
