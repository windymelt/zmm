package com.github.windymelt.zmm
package util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DictSpec extends UnitSpec with UtilComponent {
  describe("dictFromNode") {
    it("returns empty dictionary when there is no meta/dict element") {
      // TODO: dummy xml generator
      val xml =
        <content version="0.0"><meta></meta><dialogue></dialogue></content>
      Dict.dictFromNode(xml) shouldBe empty
    }

    it("returns one sized dictionary when a dictionary item passed") {
      val xml =
        <content version="0.0"><meta><dict pronounce="ズ_ンダモン">ずんだもん</dict></meta><dialogue></dialogue></content>
      val dict = Dict.dictFromNode(xml)
      dict shouldNot be(empty)
      dict should have size (1)
      dict.head shouldBe ("ずんだもん", "ズンダモン", 1)
    }

    it("returns multiple dictionary when multiple dictionary item passed") {
      val xml =
        <content version="0.0"><meta>
        <dict pronounce="ズ_ンダモン">ずんだもん</dict>
        <dict pronounce="キ_リタン">きりたん</dict>
        </meta><dialogue></dialogue></content>
      val dict = Dict.dictFromNode(xml)
      dict shouldNot be(empty)
      dict should have size (2)
      dict shouldBe Seq(
        ("ずんだもん", "ズンダモン", 1),
        ("きりたん", "キリタン", 1)
      )
    }
  }
}
