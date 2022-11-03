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

  /**
    * Writes fs2 stream into specified Path.
    *
    * @param stream
    * @param fileName
    * @return Written file path
    */
  def writeStreamToFile(stream: fs2.Stream[IO, Byte], fileName: String): IO[fs2.io.file.Path] = {
    import fs2.io.file.{Files, Path}
    val target = Path(fileName)
    stream.through(Files[IO].writeAll(target)).compile.drain.as(target)
  }
}
