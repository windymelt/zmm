package com.github.windymelt.zmm.util

object Dict {
  type Pronounce = String // Katakana
  type AccentPosition = Int
  type Dict = Seq[(String, Pronounce, AccentPosition)]

  val dictFromNode: scala.xml.Node => Dict = { elem =>
    val pronounceWithoutAccentSymbol: String => String = _.filterNot(_ == '_')
    val indexOfAccent: String => Int = _.indexOf('_')
    val dictToAccentTriple: scala.xml.Node => Seq[(String, String, Int)] =
      _.map { dictItem =>
        (
          dictItem.text,
          pronounceWithoutAccentSymbol(dictItem \@ "pronounce"),
          indexOfAccent(dictItem \@ "pronounce")
        )
      }

    (elem \ "meta" \ "dict") flatMap dictToAccentTriple
  }
}
