package com.github.windymelt.zmm.domain.repository

import cats.effect.IO
import cats.effect.kernel.Resource

trait ScreenShotComponent {
  def screenShotResource: IO[Resource[IO, ScreenShot]]

  trait ScreenShot {
    def takeScreenShot(
        htmlFilePath: os.Path,
        windowWidth: Int = 1920,
        windowHeight: Int = 1080
    ): IO[os.Path]

    /** ユーザの入力によってスクリーンショット実装が切り替わるので、それを内部で判別できるようにするための識別子。
      */
    val screenShotImplementation: String
  }
}
