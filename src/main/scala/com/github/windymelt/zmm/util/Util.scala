package com.github.windymelt.zmm.util

import cats.effect.{IO, Ref}
import cats.effect.implicits._
object Util {
  import java.security.MessageDigest
  // MessageDigestオブジェクトはアトミックに使用する必要があるが、とりあえず毎回生成することで面倒を回避する
  def sha1HexCode(bs: Array[Byte]): IO[String] = {
    val digestInstance = MessageDigest.getInstance("SHA-1")
    IO.pure(digestInstance.digest(bs).map(b => (b & 0xff).toHexString).mkString)
  }

  /** Writes fs2 stream into specified Path.
    *
    * @param stream
    * @param fileName
    * @return
    *   Written file path
    */
  def writeStreamToFile(
      stream: fs2.Stream[IO, Byte],
      fileName: String,
  ): IO[fs2.io.file.Path] = {
    import fs2.io.file.{Files, Path}
    val target = Path(fileName)
    stream.through(Files[IO].writeAll(target)).compile.drain.as(target)
  }

  import cats.{Eq, Semigroup}

  /** Seq(k -> v, k2 -> v2, ...)の形式のリストを、隣接するキーの単位で結合する。
    *
    * 例えば、a, b, a, aの形式でキーが隣接していた場合、a, b, aの形式に結合され、値はcombineされる。
    *
    * @param xs
    *   キーで結合する対象となるリスト
    * @return
    *   結合されたリスト
    */
  def groupReduction[E: Eq, S: Semigroup](xs: Seq[(E, S)]): Seq[(E, S)] = {
    import cats.syntax.eq._
    import cats.syntax.apply._
    xs.view
      .map(_.swap)
      .foldRight(Seq.empty[(S, E)]) {
        // Product2 における head <* second は、(head._1 |+| second._1, head._2)と同義 cf. productL
        case (head, second +: acc) if head._2 === second._2 =>
          (head <* second) +: acc
        case (em, acc) => em +: acc
      }
      .map(_.swap)
  }

  val hashCodeToBytes = (n: Int) => {
    import java.io.ByteArrayOutputStream
    import java.io.ObjectOutputStream
    val stream: ByteArrayOutputStream = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(stream)
    oos.writeObject(n)
    oos.close()
    stream.toByteArray
  }

  lazy val config = com.typesafe.config.ConfigFactory.load()

  given EqForPath: Eq[os.Path] = Eq.by(_.toString())
}
