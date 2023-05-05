package com.github.windymelt.zmm.domain.model

import cats.data.Kleisli
import cats.implicits._

object Filter {
  def talkingMouthFilter: Filter[Seq] = Kleisli { ctx: Context =>
    ctx.spokenVowels match {
      case None => Seq(ctx)
      case Some(vs) =>
        val ExtRe = """(.+)\.(.+)""".r.anchored

        // この関数に通すと口を閉じる
        // TODO: CharacterConfigにもtachieUrlがあるけど？
        val swapTachie: Context => Context = ctx =>
          ctx.copy(tachieUrl = ctx.tachieUrl.map {
            case ExtRe(file: String, ext: String) =>
              s"${file}!.$ext"
          })

        val spokenCtxs = vs.map { v =>
          ctx.spokenByCharacterId match {
            case None => ctx
            case Some(cid) =>
              val durModifiedCtx = ctx.copy(duration = Some(v._2))
              if (shouldCloseMouth(v._1)) {
                swapTachie(durModifiedCtx)
              } else {
                durModifiedCtx
              }
          }
        }

        // 合計Durationは元々のDurationと一致させるべく調整する
        val diff = ctx.duration.get - (vs.map(_._2).combineAll)
        val size = spokenCtxs.size
        val acc = diff / size

        spokenCtxs.map(c => c.copy(duration = c.duration.map(_ + acc)))
    }
  }

  private def shouldCloseMouth(vowel: String): Boolean =
    vowel.toLowerCase match {
      case "u"  => true
      case "i"  => true
      case "n"  => true
      case "cl" => true
      // TODO: 適宜追加のこと
      case _ => false
    }
}
