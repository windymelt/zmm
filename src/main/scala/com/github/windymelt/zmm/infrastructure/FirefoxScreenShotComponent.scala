package com.github.windymelt.zmm
package infrastructure

import cats.effect.IO
import cats.implicits._
import cats.effect.std.Mutex

trait FirefoxScreenShotComponent {
  self: domain.repository.ScreenShotComponent =>

  object FirefoxScreenShot {
    sealed trait Verbosity
    final object Quiet extends Verbosity
    final object Verbose extends Verbosity
  }

  type Path = os.Path
  def screenShot: IO[FirefoxScreenShot]

  class FirefoxScreenShot(
      firefoxCommand: String,
      verbosity: FirefoxScreenShot.Verbosity,
      mutex: Mutex[IO] // firefox outputs fixed "screenshot.png", so we cannot call it concurrently
  ) extends ScreenShot {
    val stdout = verbosity match {
      case FirefoxScreenShot.Quiet   => os.Pipe
      case FirefoxScreenShot.Verbose => os.Inherit
    }
    def takeScreenShot(
        htmlFilePath: Path,
        windowWidth: Int = 1920,
        windowHeight: Int = 1080
    ): IO[Path] = {
      val absPath = htmlFilePath
      val fileUri = s"file://$absPath"
      println(fileUri)
      val proc = os.proc(
        firefoxCommand,
        "-headless",
        "-screenshot",
        "-window-size",
        s"$windowWidth,$windowHeight",
        fileUri
      )
      mutex.lock.surround {
        IO.blocking {
          proc.call(stdout = stdout, stderr = stdout, cwd = os.pwd)
          val outputPath = os.Path(s"${htmlFilePath}.png")
          os.move(os.pwd / "screenshot.png", outputPath, replaceExisting = true)
          outputPath
        }
      }
    }
  }
}
