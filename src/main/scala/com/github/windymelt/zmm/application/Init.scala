package com.github.windymelt.zmm.application

import cats.effect.IO

object Init {
  /** 現在のディレクトリをZMMプロジェクトとして初期化する。
   *
   * 現在のディレクトリに原稿XMLファイルや生成物配置用のディレクトリを作成する。 既にディレクトリやファイルが存在している場合は何もしない。
   *
   * @return
   *   Unitを返す。
   */
  def initializeProject(): IO[Unit] = {
    val agreed = for {
      cwd <- IO.pure(os.pwd.toString())
      _ <- IO.print(s"$cwd を ZMMプロジェクトとして初期化しますか? [y/N]?>")
      ynString <- IO.readLine
    } yield ynString == "y"

    val placeXml: IO[Unit] = IO {
      os.exists(os.pwd / "script.xml") match {
        case true  => IO.println("script.xml は既に存在するのでスキップされました")
        case false => IO(os.write(os.pwd / "script.xml", xml.script().body))
      }
    }.flatten

    val digArtifacts: IO[Unit] = IO {
      os.exists(os.pwd / "artifacts") match {
        case true  => IO.println("artifacts/ は既に存在するのでスキップされました")
        case false => IO(os.makeDir(os.pwd / "artifacts"))
      }
    }.flatten

    val digArtifactsHtml: IO[Unit] = IO {
      os.exists(os.pwd / "artifacts" / "html") match {
        case true  => IO.println("artifacts/html/ は既に存在するのでスキップされました")
        case false => IO(os.makeDir(os.pwd / "artifacts" / "html"))
      }
    }.flatten

    val digAssets: IO[Unit] = IO {
      os.exists(os.pwd / "assets") match {
        case true  => IO.println("assets/ は既に存在するのでスキップされました")
        case false => IO(os.makeDir(os.pwd / "assets"))
      }
    }.flatten

    val init = for {
      _ <- placeXml
      _ <- digArtifacts >> digArtifactsHtml
      _ <- digAssets
    } yield ()

    // ZMMプロジェクトを構成するいくつかのファイル/ディレクトリについて、存在しなかったらテンプレートをもとに作成する、を繰り返す
    agreed flatMap {
      case true  => init
      case false => IO.println("中断します")
    }
  }
}
