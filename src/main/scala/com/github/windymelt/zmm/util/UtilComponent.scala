package com.github.windymelt.zmm.util

import cats.effect.{IO, Ref}
import cats.effect.implicits._
trait UtilComponent {
  import java.security.MessageDigest
  // MessageDigestオブジェクトはアトミックに使用する必要があるが、とりあえず毎回生成することで面倒を回避する
  def sha1HexCode(bs: Array[Byte]): IO[String] = {
    val digestInstance = MessageDigest.getInstance("SHA-1")
    IO.pure(digestInstance.digest(bs).map(b => (b & 0xff).toHexString).mkString)
  }
}
