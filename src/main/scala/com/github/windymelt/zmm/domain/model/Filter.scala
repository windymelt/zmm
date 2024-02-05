package com.github.windymelt.zmm
package domain.model

import cats.data.Kleisli
import cats.implicits._
import scala.jdk.CollectionConverters.ConcurrentMapHasAsScala

object Filter {
  def talkingMouthFilter: Filter[Seq] = Kleisli { (ctx: Context) =>
    ctx.spokenVowels match {
      case None => Seq(ctx)
      case Some(vs) =>
        val ExtRe = """(.+)\.(.+)""".r.anchored

        // この関数に通すと口を閉じる
        // 口を閉じている立ち絵のファイルが存在する場合はそれを利用する。存在しない場合は元々の画像にフォールバックする
        // TODO: CharacterConfigにもtachieUrlがあるけど？
        val swapTachie: Context => Context = ctx =>
          ctx.copy(tachieUrl = ctx.tachieUrl.map {
            case originalPath @ ExtRe(file: String, ext: String) =>
              val alternativePath = s"${file}!.$ext"
              alternativeFileIfExists(originalPath, alternativePath)
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

  private val ExtRe = """(.+)\.(.+)""".r.anchored

  // caution: invokes side effect.
  private def alternativeFileIfExists(
      originalPath: String,
      alternativePath: String
  ): String = originalPath match {
    case originalPath @ ExtRe(file: String, ext: String) =>
      tachieExistenceCache.get(alternativePath) match {
        case Some(true)  => alternativePath
        case Some(false) => originalPath
        case None =>
          val pathToFind = os.pwd / os.RelPath(
            util.PathAlias.resolve(
              alternativePath,
              "ffmpeg" /* ffmpeg refers `./`. we should define stub Purpose */
            )
          )
          if (os.exists(pathToFind)) {
            tachieExistenceCache += alternativePath -> true
            alternativePath
          } else {
            // fallback to original path.
            tachieExistenceCache += alternativePath -> false
            originalPath
          }
      }
    case _ => ??? // should not be reached
  }

  private val tachieExistenceCache =
    new java.util.concurrent.ConcurrentHashMap[String, Boolean]().asScala

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
