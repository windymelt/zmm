package com.github.windymelt.zmm
package infrastructure

import cats.effect.IO

object ChromeScreenShot {
  sealed trait Verbosity
  object Quiet extends Verbosity
  object Verbose extends Verbosity
}

class ChromeScreenShot(
    chromeCommand: String,
    verbosity: ChromeScreenShot.Verbosity,
    noSandBox: Boolean = false,
) extends domain.repository.ScreenShot {
  val screenShotImplementation = "chrome"
  val stdout = verbosity match {
    case ChromeScreenShot.Quiet   => os.Pipe
    case ChromeScreenShot.Verbose => os.Inherit
  }
  def takeScreenShot(
      htmlFilePath: os.Path,
      windowWidth: Int = 1920,
      windowHeight: Int = 1080,
  ): IO[os.Path] = IO.delay {
    val proc =
      if noSandBox then
        os.proc(
          chromeCommand,
          "--headless=new",
          "--no-sandbox",
          "--hide-scrollbars",
          s"--screenshot=$htmlFilePath.png",
          s"--window-size=$windowWidth,$windowHeight",
          "--default-background-color=00000000",
          htmlFilePath,
        )
      else
        os.proc(
          chromeCommand,
          "--headless=new",
          "--hide-scrollbars",
          s"--screenshot=$htmlFilePath.png",
          s"--window-size=$windowWidth,$windowHeight",
          "--default-background-color=00000000",
          htmlFilePath,
        )
    proc.call(stdout = stdout, stderr = stdout, cwd = os.pwd)
  } *> IO.pure(os.Path(s"$htmlFilePath.png"))
}
