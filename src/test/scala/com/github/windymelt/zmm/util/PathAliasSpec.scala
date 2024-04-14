package com.github.windymelt.zmm.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PathAliasSpec extends AnyFlatSpec with Matchers {

  "PathAlias" should "resolve @assets" in {
    PathAlias.resolve("./do/not/modify", "template") shouldBe "./do/not/modify"

    PathAlias.resolve(
      "@assets/foo/bar.png",
      "template",
    ) shouldBe "../../assets/foo/bar.png"

    PathAlias.resolve(
      "@assets/foo/bar.mp3",
      "ffmpeg",
    ) shouldBe "./assets/foo/bar.mp3"
  }
}
