package com.github.windymelt.zmm
package infrastructure

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.effect.std.Mutex
import cats.implicits._

import scala.concurrent.duration.FiniteDuration

trait FirefoxScreenShotComponent {
  self: domain.repository.ScreenShotComponent =>

  object FirefoxScreenShot {
    sealed trait Verbosity
    object Quiet extends Verbosity
    object Verbose extends Verbosity
  }

  def screenShotResource: IO[Resource[IO, ScreenShot]]

  class FirefoxScreenShot(
      firefoxCommand: String,
      verbosity: FirefoxScreenShot.Verbosity,
  ) extends ScreenShot {
    val screenShotImplementation = "firefox"
    val stdout = verbosity match {
      case FirefoxScreenShot.Quiet   => os.Pipe
      case FirefoxScreenShot.Verbose => os.Inherit
    }
    def takeScreenShot(
        htmlFilePath: os.Path,
        windowWidth: Int = 1920,
        windowHeight: Int = 1080,
    ): IO[os.Path] = {
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
      // mutex.lock.surround {
      IO.blocking {
        proc.call(stdout = stdout, stderr = stdout, cwd = os.pwd)
        val outputPath = os.Path(s"${htmlFilePath}.png")
        os.move(os.pwd / "screenshot.png", outputPath, replaceExisting = true)
        outputPath
      }
      //  }
    }
  }
}
