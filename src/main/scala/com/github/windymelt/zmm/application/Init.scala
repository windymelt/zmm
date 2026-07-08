package com.github.windymelt.zmm.application

import zio.Console
import zio.Task
import zio.ZIO

object Init {

  /** 現在のディレクトリをZMMプロジェクトとして初期化する。
    *
    * 現在のディレクトリに原稿XMLファイルや生成物配置用のディレクトリを作成する。 既にディレクトリやファイルが存在している場合は何もしない。
    *
    * @return
    *   Unitを返す。
    */
  def initializeProject(): Task[Unit] = {
    val agreed = for {
      cwd <- ZIO.succeed(os.pwd.toString())
      _ <- Console.print(s"$cwd を ZMMプロジェクトとして初期化しますか? [y/N]?>")
      ynString <- Console.readLine
    } yield ynString == "y"

    val placeXml: Task[Unit] =
      ZIO.attemptBlocking(os.exists(os.pwd / "script.xml")).flatMap {
        case true => Console.printLine("script.xml は既に存在するのでスキップされました")
        case false =>
          ZIO.attemptBlocking(os.write(os.pwd / "script.xml", xml.script().body))
      }

    val digArtifacts: Task[Unit] =
      ZIO.attemptBlocking(os.exists(os.pwd / "artifacts")).flatMap {
        case true => Console.printLine("artifacts/ は既に存在するのでスキップされました")
        case false => ZIO.attemptBlocking(os.makeDir(os.pwd / "artifacts"))
      }

    val digArtifactsHtml: Task[Unit] =
      ZIO.attemptBlocking(os.exists(os.pwd / "artifacts" / "html")).flatMap {
        case true => Console.printLine("artifacts/html/ は既に存在するのでスキップされました")
        case false =>
          ZIO.attemptBlocking(os.makeDir(os.pwd / "artifacts" / "html"))
      }

    val digAssets: Task[Unit] =
      ZIO.attemptBlocking(os.exists(os.pwd / "assets")).flatMap {
        case true  => Console.printLine("assets/ は既に存在するのでスキップされました")
        case false => ZIO.attemptBlocking(os.makeDir(os.pwd / "assets"))
      }

    val init = for {
      _ <- placeXml
      _ <- digArtifacts *> digArtifactsHtml
      _ <- digAssets
    } yield ()

    // ZMMプロジェクトを構成するいくつかのファイル/ディレクトリについて、存在しなかったらテンプレートをもとに作成する、を繰り返す
    agreed flatMap {
      case true  => init
      case false => Console.printLine("中断します")
    }
  }
}
