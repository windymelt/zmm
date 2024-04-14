package com.github.windymelt.zmm

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import org.http4s.syntax.header

import java.io.OutputStream

object Main
    extends CommandIOApp(
      name = "zmm",
      header =
        "Zunda Movie Maker -- see https://www.3qe.us/zmm/doc/ for more documentation",
    ) {
  override def main: Opts[IO[ExitCode]] = CliOptions.opts map { o =>
    val defaultCli = new ChromiumCli(logLevel = "INFO")

    o match {
      case VersionFlag() => defaultCli.showVersion >> IO.pure(ExitCode.Success)
      case ShowCommand(target) =>
        target match {
          case "voicevox" =>
            defaultCli.showVoiceVoxSpeakers() >> IO.pure(ExitCode.Success)
          case _ =>
            IO.println(
              "subcommand [show] only accepts 'voicevox'. try `show voicevox`",
            ) >> IO.pure(ExitCode.Error)
        }
      case Generate(file, out, screenShotBackend, verbosity) =>
        val optionalLogLevel = verbosityToLogLevel(
          vCount = verbosity.getOrElse(0),
          qCount = 0, /* TODO: implement it later */
        )
        val environmentalLogLevel = getLogLevelFromEnvVar()
        val logLevel = environmentalLogLevel.getOrElse(optionalLogLevel)
        setLogLevel(logLevel)

        val cli = screenShotBackend match {
          // TODO: ffmpeg verbosityをcli opsから設定可能にする
          case Some(ScreenShotBackend.Chrome) =>
            new ChromiumCli(logLevel = logLevel)
          case Some(ScreenShotBackend.Firefox) =>
            new FirefoxCli(logLevel = logLevel)
          case _ => new ChromiumCli(logLevel = logLevel)
        }
        cli.logger.debug(s"Verbose mode enabled (log level: $logLevel)") >> cli
          .generate(
            file.target.toString,
            out.toAbsolutePath.toString,
          ) >>
          IO.pure(ExitCode.Success)
      case InitializeCommand() =>
        application.Init.initializeProject() >> IO.pure(ExitCode.Success)
    }
  }

  /** ログレベルを実際にlogbackに適用する。
    *
    * @param level
    *   ログレベルを表現する文字列。e.g. ERROR
    */
  private def setLogLevel(level: String): Unit = {
    // https://stackoverflow.com/a/3838108
    import org.slf4j.LoggerFactory
    import ch.qos.logback.classic.Level
    import ch.qos.logback.classic.Logger

    val root: Logger =
      LoggerFactory
        .getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME)
        .asInstanceOf[Logger]
    root.setLevel(Level.toLevel(level));
  }

  /** verbose/quietオプションの個数に従ってlogbackに適用するログレベルを決定する。
    *
    * @param vCount
    * --verboseの個数
    * @param qCount
    * --quietの個数
    * @return
    */
  private def verbosityToLogLevel(vCount: Int, qCount: Int): String =
    vCount - qCount match {
      case n if n <= -2 => "ERROR"
      case -1           => "WARN"
      case 0            => "INFO"
      case 1            => "DEBUG"
      case n if n >= 2  => "TRACE"
    }

  private def getLogLevelFromEnvVar(): Option[String] = sys.env.get("LOG_LEVEL")
}
