package com.github.windymelt.zmm.domain.repository

import cats.effect.IO

trait ScreenShotComponent {
  type Path
  def screenShot: ScreenShot

  trait ScreenShot {
    def takeScreenShot(htmlFilePath: Path, windowWidth: Int, windowHeight: Int): IO[Path]
  }
}

