package com.github.windymelt.zmm.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UtilSpec extends AnyFlatSpec with Matchers with UtilComponent {
  import cats.implicits._
  "groupReduction" should "reduce empty Seq as empty Seq" in {
    groupReduction[String, Int](Seq.empty) shouldBe Seq()
  }

  it should "reduce singleton Seq as-is" in {
    groupReduction(Seq("foo" -> 42)) shouldBe Seq("foo" -> 42)
  }

  it should "reduce same key" in {
    groupReduction(Seq("foo" -> 1, "foo" -> 2)) shouldBe Seq("foo" -> 3)
  }

  it should "not reduce same key iff not adjacent" in {
    groupReduction(Seq("foo" -> 1, "bar" -> 2, "foo" -> 3)) shouldBe Seq("foo" -> 1, "bar" -> 2, "foo" -> 3)
  }

  it should "treat whatever that is a Eq and Monoid" in {
    val xs: Seq[(Boolean, Int)] = Seq(true -> 42, true -> 10, false -> 5, false -> 5, true -> 20)
    val expected: Seq[(Boolean, Int)] = Seq(true -> 52, false -> 10, true -> 20)
    groupReduction(xs) shouldBe expected
  }

  "EqForPath" should "distinguish / and /foo" in {
    import os.Path
    cats.Eq[Path].eqv(Path("/"), Path("/")) shouldBe true
    cats.Eq[Path].eqv(Path("/"), Path("/foo")) shouldBe false
  }
}

