package com.github.windymelt.zmm.util

trait UtilComponent {
  def sha1HexCode(bs: Array[Byte]): String = synchronized {
    UtilComponent.md.digest(bs).map(b => (b & 0xff).toHexString).mkString
  }
}

object UtilComponent {
  lazy val md = java.security.MessageDigest.getInstance("SHA-1")
}
