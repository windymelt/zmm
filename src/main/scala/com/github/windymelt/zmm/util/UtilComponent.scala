package com.github.windymelt.zmm.util

import cats.effect.{IO, Ref}
import cats.effect.implicits._
trait UtilComponent {
  def sha1HexCode(bs: Array[Byte]): IO[String] = for {
    mdRef <- UtilComponent.md
    md <- mdRef.get
  } yield md.digest(bs).map(b => (b & 0xff).toHexString).mkString
}

object UtilComponent {
  import java.security.MessageDigest
  // MessageDigestオブジェクトはアトミックに使用する必要があるためRefを使ってアトミック操作を確保する
  lazy val md: IO[Ref[IO, MessageDigest]] =
    Ref[IO].of(MessageDigest.getInstance("SHA-1"))
}
