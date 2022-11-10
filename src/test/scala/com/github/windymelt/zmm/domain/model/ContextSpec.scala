package com.github.windymelt.zmm.domain.model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ContextSpec extends AnyFlatSpec with Matchers {
  // Verify that Context is a Monoid
  "Context" should "satisfy associative law" in {
    val x = Context(
      Map("zundamon" -> VoiceVoxBackendConfig("3")),
      Map("z" -> CharacterConfig("z", "zundamon")),
      Some("https://example.com/bg1.png")
    )
    val y = Context(
      Map("metan" -> VoiceVoxBackendConfig("2")),
      Map("m" -> CharacterConfig("m", "metan")),
      Some("https://example.com/bg2.png")
    )
    val z = Context(
      Map("tsumugi" -> VoiceVoxBackendConfig("8")),
      Map("t" -> CharacterConfig("t", "tsumugi")),
      None
    )

    import cats.implicits._
    { (x |+| y) |+| z } shouldEqual { x |+| (y |+| z) }
  }

  it should "have identity element" in {
    val x = Context(
      Map("zundamon" -> VoiceVoxBackendConfig("3")),
      Map("z" -> CharacterConfig("z", "zundamon"))
    )

    val e = Context.empty

    import cats.implicits._
    (x |+| e) shouldEqual (e |+| x)
  }
}
