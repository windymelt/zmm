package com.github.windymelt.zmm

import com.monovore.decline.Command
import zio.Console
import zio.ExitCode
import zio.Runtime
import zio.Task
import zio.ZIO
import zio.ZIOAppDefault
import zio.logging.backend.SLF4J

object Main extends ZIOAppDefault {
  override val bootstrap =
    Runtime.removeDefaultLoggers >>> SLF4J.slf4j

  private val command: Command[ZmmOption] = Command(
    name = "zmm",
    header =
      "Zunda Movie Maker -- see https://www.3qe.us/zmm/doc/ for more documentation",
  )(CliOptions.opts)

  override def run: ZIO[zio.ZIOAppArgs, Nothing, Unit] =
    getArgs.flatMap { args =>
      command.parse(args, sys.env) match {
        case Left(help) =>
          // --help による表示も Left(Help) で返るため、エラーが空なら正常終了とする
          val code =
            if (help.errors.isEmpty) ExitCode.success else ExitCode.failure
          Console.printLineError(help.toString).orDie *> exit(code)
        case Right(o) =>
          dispatch(o).foldZIO(
            err =>
              Console.printLineError(String.valueOf(err.getMessage)).orDie *>
                exit(ExitCode.failure),
            exit,
          )
      }
    }

  private def dispatch(o: ZmmOption): Task[ExitCode] =
    o match {
      case VersionFlag() =>
        ZIO
          .serviceWithZIO[Cli](_.showVersion)
          .provide(Design.chrome(util.Util.config))
          .as(ExitCode.success)

      case ShowCommand(target) =>
        target match {
          case "voicevox" =>
            ZIO
              .serviceWithZIO[Cli](_.showVoiceVoxSpeakers())
              .provide(Design.chrome(util.Util.config))
              .as(ExitCode.success)

          case _ =>
            Console.printLine(
              "subcommand [show] only accepts 'voicevox'. try `show voicevox`",
            ) *> ZIO.succeed(ExitCode.failure)
        }
      case Generate(file, out, screenShotBackend, verbosity) =>
        val optionalLogLevel = verbosityToLogLevel(
          vCount = verbosity.getOrElse(0),
          qCount = 0, /* TODO: implement it later */
        )
        val environmentalLogLevel = getLogLevelFromEnvVar
        val logLevel = environmentalLogLevel.getOrElse(optionalLogLevel)
        setLogLevel(logLevel)

        val cliLayer = screenShotBackend match
          // TODO: ffmpeg verbosityをcli opsから設定可能にする
          case Some(ScreenShotBackend.Firefox) =>
            Design.firefox(util.Util.config, logLevel)
          case _ =>
            Design.chrome(util.Util.config, logLevel)

        ZIO
          .serviceWithZIO[Cli] { cli =>
            ZIO.logDebug(s"Verbose mode enabled (log level: $logLevel)") *>
              cli.generate(file.target.toString, out.toAbsolutePath.toString)
          }
          .tapErrorCause(cause => ZIO.logError(cause.prettyPrint))
          .provide(cliLayer)
          .as(ExitCode.success)

      case InitializeCommand() =>
        application.Init.initializeProject().as(ExitCode.success)
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

    @SuppressWarnings(
      Array(
        "scalafix:DisableSyntax.asInstanceOf",
      ),
    )
    val root: Logger =
      LoggerFactory
        .getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME)
        .asInstanceOf[Logger]

    root.setLevel(Level.toLevel(level))
  }

  /** verbose/quietオプションの個数に従ってlogbackに適用するログレベルを決定する。
    *
    * @param vCount
    *   --verboseの個数
    * @param qCount
    *   --quietの個数
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

  private def getLogLevelFromEnvVar: Option[String] = sys.env.get("LOG_LEVEL")
}
