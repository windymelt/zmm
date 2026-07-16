package com.github.windymelt.zmm
package infrastructure

import zio.Task
import zio.ZIO

object DockerFFmpeg {

  /** jar に同梱した Dockerfile-ffmpeg から ffmpeg 実行用の Docker イメージをビルドする。
    *
    * Dockerfile は ADD で静的ビルドの ffmpeg を取得するだけでビルドコンテキストを必要としないため、
    * 標準入力経由でビルドする。2回目以降は Docker のレイヤーキャッシュが効くため高速に完了する。
    *
    * @param imageTag
    *   ビルドするイメージのタグ
    * @param verbosity
    *   ビルドログの出力有無
    */
  def buildImage(
      imageTag: String,
      verbosity: ConcreteFFmpeg.Verbosity,
  ): Task[Unit] = ZIO.attemptBlocking {
    val dockerfile = scala.io.Source
      .fromInputStream(
        getClass().getClassLoader().getResourceAsStream("Dockerfile-ffmpeg"),
      )
      .mkString
    val out = verbosity match {
      case ConcreteFFmpeg.Quiet   => os.Pipe
      case ConcreteFFmpeg.Verbose => os.Inherit
    }
    os.proc("docker", "build", "-t", imageTag, "-")
      .call(stdin = dockerfile, stdout = out, stderr = out, cwd = os.pwd)
  }.unit
}

/** Dockerfile-ffmpeg からビルドしたイメージ上で ffmpeg / ffprobe を実行する実装。
  *
  * ホストに ffmpeg をインストールしなくても動画生成できるようにするための実装。 カレントディレクトリをコンテナ内の同一パスにマウントすることで、
  * cutfile 等に記述された絶対パスをコンテナ内でもそのまま解決できるようにしている。
  */
class DockerFFmpeg(
    imageTag: String,
    verbosity: ConcreteFFmpeg.Verbosity,
) extends ConcreteFFmpeg("ffmpeg", verbosity) {
  // コンテナがrootでファイルを書くとホスト側から上書きできなくなるため、ホストと同じUID/GIDで実行する
  private lazy val hostUidGid: String = {
    val uid = os.proc("id", "-u").call().out.text().trim
    val gid = os.proc("id", "-g").call().out.text().trim
    s"$uid:$gid"
  }

  private def dockerRunPrefix: Seq[os.Shellable] = Seq[os.Shellable](
    "docker",
    "run",
    "--rm",
    "-i", // concatenateWavFiles が標準入力を渡すため必要
    "--user",
    hostUidGid,
    // SELinux 環境でマウントしたファイルへのアクセスが拒否されないようにする。
    // :z と異なりホスト側ファイルのラベルを書き換えない。SELinux 無効の環境では単に無視される
    "--security-opt",
    "label=disable",
    "-v",
    s"${os.pwd}:${os.pwd}",
    "-w",
    s"${os.pwd}",
    imageTag,
  )

  override protected def ffmpegBaseCommand: Seq[os.Shellable] =
    dockerRunPrefix :+ ("ffmpeg": os.Shellable)
  override protected def ffprobeBaseCommand: Seq[os.Shellable] =
    dockerRunPrefix :+ ("ffprobe": os.Shellable)
}
