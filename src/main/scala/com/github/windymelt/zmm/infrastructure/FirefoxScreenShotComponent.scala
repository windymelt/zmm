package com.github.windymelt.zmm
package infrastructure

import zio.Task
import zio.ZIO

object FirefoxScreenShot {
  sealed trait Verbosity
  object Quiet extends Verbosity
  object Verbose extends Verbosity
}

class FirefoxScreenShot(
    firefoxCommand: String,
    verbosity: FirefoxScreenShot.Verbosity,
) extends domain.repository.ScreenShot {
  val screenShotImplementation = "firefox"
  val stdout = verbosity match
    case FirefoxScreenShot.Quiet   => os.Pipe
    case FirefoxScreenShot.Verbose => os.Inherit

  def takeScreenShot(
      htmlFilePath: os.Path,
      windowWidth: Int = 1920,
      windowHeight: Int = 1080,
  ): Task[os.Path] = {
    val absPath = htmlFilePath
    val fileUri = s"file://$absPath"
    val proc = os.proc(
      firefoxCommand,
      "-headless",
      "-screenshot",
      "-window-size",
      s"$windowWidth,$windowHeight",
      fileUri,
    )
    ZIO.attemptBlocking {
      proc.call(stdout = stdout, stderr = stdout, cwd = os.pwd)
      val outputPath = os.Path(s"$htmlFilePath.png")
      os.move(os.pwd / "screenshot.png", outputPath, replaceExisting = true)
      outputPath
    }
  }
}
