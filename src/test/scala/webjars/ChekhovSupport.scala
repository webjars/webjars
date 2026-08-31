package webjars

import chekhov.ChekhovConfig
import chekhov.protocol.PinnedPlaywright
import zio.ULayer

import java.io.File
import java.nio.file.Files
import java.util.concurrent.TimeUnit

object ChekhovSupport:

  enum BrowserEnv derives CanEqual:
    case NixOS, GitHubActions, Generic

  object BrowserEnv:
    def detect(
        env: Map[String, String] = sys.env,
        fileExists: String => Boolean = p => File(p).exists(),
        osReleaseId: => Option[String] = osReleaseId(),
    ): BrowserEnv =
      if isNixOs(fileExists, osReleaseId) then NixOS
      else if env.get("GITHUB_ACTIONS").contains("true") then GitHubActions
      else Generic

    private def isNixOs(fileExists: String => Boolean, osReleaseId: => Option[String]): Boolean =
      fileExists("/etc/NIXOS") || osReleaseId.contains("nixos")

    private def osReleaseId(): Option[String] =
      try
        val file = File("/etc/os-release")
        if !file.canRead then None
        else
          Files
            .readAllLines(file.toPath)
            .stream()
            .filter(_.startsWith("ID="))
            .map(_.stripPrefix("ID=").trim.stripPrefix("\"").stripSuffix("\"").toLowerCase)
            .findFirst()
            .map[Option[String]](Some(_))
            .orElse(None)
      catch case _: Throwable => None
  end BrowserEnv

  private def sh(command: String): Option[String] =
    try
      val process = ProcessBuilder("sh", "-c", command).redirectErrorStream(true).start()
      val output  = String(process.getInputStream.readAllBytes).trim
      if process.waitFor(15, TimeUnit.SECONDS) && process.exitValue == 0 && output.nonEmpty then Some(output) else None
    catch case _: Throwable => None

  private def which(command: String): Option[String] = sh(s"command -v $command")

  lazy val browserExecutable: Option[String] =
    sys.env.get("WEBJARS_PW_CHROME").filter(path => File(path).canExecute)
      .orElse(
        List("chromium", "chromium-browser", "google-chrome", "google-chrome-stable")
          .iterator.flatMap(which).nextOption()
      )

  private lazy val nodeAvailable: Boolean = which("node").isDefined && which("npm").isDefined

  private lazy val driverReady: Boolean =
    try
      val cli = PinnedPlaywright.cliInCache()
      if Files.isRegularFile(cli) then true
      else
        val packageDir = PinnedPlaywright.packageDir()
        Files.createDirectories(packageDir)
        Files.writeString(packageDir.resolve("package.json"), """{"name":"webjars-chekhov-pw","private":true}""")
        val processBuilder = ProcessBuilder(
          "npm",
          "install",
          "--no-audit",
          "--no-fund",
          s"playwright@${PinnedPlaywright.version}",
        ).directory(packageDir.toFile).redirectErrorStream(true)
        processBuilder.environment().put("PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD", "1")
        val process = processBuilder.start()
        val _       = String(process.getInputStream.readAllBytes)
        val ok      = process.waitFor(300, TimeUnit.SECONDS) && process.exitValue == 0
        ok && Files.isRegularFile(cli)
    catch case _: Throwable => false

  lazy val env: BrowserEnv = BrowserEnv.detect()

  lazy val available: Boolean = nodeAvailable && browserExecutable.isDefined && driverReady

  def configFor(env: BrowserEnv, browser: Option[String]): ChekhovConfig =
    env match
      case BrowserEnv.NixOS | BrowserEnv.Generic =>
        ChekhovConfig(
          headless       = true,
          executablePath = browser,
          launchArgs     = List("--no-sandbox", "--disable-gpu"),
        )
      case BrowserEnv.GitHubActions =>
        ChekhovConfig(
          headless = true,
          channel  = Some("chrome"),
        )

  def config: ChekhovConfig = configFor(env, browserExecutable)

  def configLayer: ULayer[ChekhovConfig] = ChekhovConfig.layer(config)
