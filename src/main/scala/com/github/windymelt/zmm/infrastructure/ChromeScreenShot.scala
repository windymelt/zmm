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

  class ChromeScreenShot(chromeCommand: String, verbosity: ChromeScreenShot.Verbosity, noSandBox: Boolean = false, noGpu: Boolean = false) extends ScreenShot {
    import os.Shellable._
    val stdout = verbosity match {
      case ChromeScreenShot.Quiet => os.Pipe
      case ChromeScreenShot.Verbose => os.Inherit
    }
    private val chromiumCommands: Seq[os.Shellable] = {
      var cmd = collection.mutable.Seq[os.Shellable](chromeCommand, "--headless")
      if (noSandBox) cmd = cmd :+ "--no-sandbox"
      if (noGpu) cmd = cmd :+ "--no-gpu"
      cmd.toSeq
    }
    def takeScreenShot(htmlFilePath: Path, windowWidth: Int = 1920, windowHeight: Int = 1080): IO[Path] = IO.delay {
      val screenshotCommand: Seq[os.Shellable] = chromiumCommands ++ Seq(s"--screenshot=${htmlFilePath}.png", s"--window-size=${windowWidth},${windowHeight}", htmlFilePath.toString)
      os.proc(screenshotCommand: _*).call(stdout = stdout, stderr = stdout, cwd = os.pwd)
    } >> IO.pure(os.Path(s"${htmlFilePath}.png"))
  }
}

