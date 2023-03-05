package com.github.windymelt.zmm.domain.model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

// TODO: use UnitSpec
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

  import scala.xml._
  "Context.fromNode" should "extract say=node pair from empty elems" in {
    val e = <dialogue />
    Context.fromNode(e) shouldBe empty
  }

  it should "extract say=node pair from one say" in {
    val e = <dialogue><say>こんにちは</say></dialogue>
    val c = Context.fromNode(e)
    c should have size (1)
    c.head._1.text shouldBe "こんにちは"
    c.head._2.voiceConfigMap shouldBe empty
    c.head._2.characterConfigMap shouldBe empty
    c.head._2.backgroundImageUrl shouldBe empty
  }

  it should "split say by empty p tag" in {
    val e = <dialogue><say motif="motif0">Hello<p/>world</say></dialogue>
    val c = Context.fromNode(e)
    c should have size (2)
    c(0)._1.text shouldBe "Hello"
    c(1)._1.text shouldBe "world"

    val e0 =
      <dialogue><say motif="motif0">Hello</say><say motif="motif0">world</say></dialogue>
    val c0 = Context.fromNode(e0)

    c shouldEqual c0
  }

  it should "recognize context propergation" in {
    val e =
      <dialogue backgroundImage="https://example.com/default.png">
        <scene backgroundImage="https://example.com/bg1.png">
          <!-- ここではbg1.pngが伝播してくる -->
          <say>こんにちはなのだ</say>
          <say>一つめのコンテキストなのだ</say>
        </scene>
        <scene>
          <!-- ここではdefault.pngが伝播してくる -->
          <say>そして、二つめのコンテキストだよ</say>
        </scene>
     </dialogue>

    val cs = Context.fromNode(e)
    cs should have size (3)
    cs.map(_._1.text) shouldBe Seq(
      "こんにちはなのだ",
      "一つめのコンテキストなのだ",
      "そして、二つめのコンテキストだよ"
    )
    cs.map(_._2.backgroundImageUrl) shouldBe Seq(
      Some("https://example.com/bg1.png"),
      Some("https://example.com/bg1.png"),
      Some("https://example.com/default.png")
    )
  }
}
