package com.github.windymelt.zmm
package util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

// TODO: use UnitSpec
class UtilSpec extends UnitSpec {
  import cats.implicits._
  describe("groupReduction") {
    it("should reduce empty Seq as empty Seq") {
      Util.groupReduction[String, Int](Seq.empty) shouldBe Seq()
    }

    it("should reduce singleton Seq as-is") {
      Util.groupReduction(Seq("foo" -> 42)) shouldBe Seq("foo" -> 42)
    }

    it("should reduce same key") {
      Util.groupReduction(Seq("foo" -> 1, "foo" -> 2)) shouldBe Seq("foo" -> 3)
    }

    it("should not reduce same key iff not adjacent") {
      Util.groupReduction(Seq("foo" -> 1, "bar" -> 2, "foo" -> 3)) shouldBe Seq(
        "foo" -> 1,
        "bar" -> 2,
        "foo" -> 3,
      )
    }

    it("treat whatever that is a Eq and Monoid") {
      val xs: Seq[(Boolean, Int)] =
        Seq(true -> 42, true -> 10, false -> 5, false -> 5, true -> 20)
      val expected: Seq[(Boolean, Int)] =
        Seq(true -> 52, false -> 10, true -> 20)
      Util.groupReduction(xs) shouldBe expected
    }
  }

  describe("EqForPath") {
    it("should distinguish / and /foo") {
      import os.Path
      import Util.EqForPath
      cats.Eq[Path].eqv(Path("/"), Path("/")) shouldBe true
      cats.Eq[Path].eqv(Path("/"), Path("/foo")) shouldBe false
    }
  }
}
