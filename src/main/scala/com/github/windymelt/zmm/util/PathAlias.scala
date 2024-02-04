package com.github.windymelt.zmm.util

/** {{{@assets/}}}
  * などのエイリアス記法を変換する機構。これによりHTMLレンダリングやffmpegのBGM合成などの文脈からXMLファイルのパス記述を切り離し、処理プロセスがユーザから隠蔽される。
  */
object PathAlias {
  type Purpose = "template" | "ffmpeg"
  def resolve(aliasPath: String, purpose: Purpose): String = aliasPath match {
    case AliasRe(annotation, rest) =>
      annotationPathMap(annotation)(purpose) ++ rest
    case _ => aliasPath
  }
  private val AliasRe = """@([^/]+)/(.+)""".r.anchored
  private val annotationPathMap: Map[String, Map[Purpose, String]] = Map(
    "assets" -> Map(
      "template" -> "../../assets/",
      "ffmpeg" -> "./assets/"
    )
  )
}
