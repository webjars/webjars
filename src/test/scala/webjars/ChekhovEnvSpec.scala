package webjars

import webjars.ChekhovSupport.BrowserEnv
import zio.test.*

object ChekhovEnvSpec extends ZIOSpecDefault:

  private def detect(
      env: Map[String, String] = Map.empty,
      files: Set[String] = Set.empty,
      osId: Option[String] = None,
  ): BrowserEnv =
    BrowserEnv.detect(env = env, fileExists = files.contains, osReleaseId = osId)

  def spec = suite("ChekhovEnv")(
    suite("detect")(
      test("detects NixOS from its marker file") {
        assertTrue(detect(files = Set("/etc/NIXOS")) == BrowserEnv.NixOS)
      },
      test("detects GitHub Actions outside NixOS") {
        assertTrue(detect(env = Map("GITHUB_ACTIONS" -> "true")) == BrowserEnv.GitHubActions)
      },
      test("prefers NixOS on a self-hosted GitHub runner") {
        assertTrue(
          detect(env = Map("GITHUB_ACTIONS" -> "true"), files = Set("/etc/NIXOS")) == BrowserEnv.NixOS
        )
      },
      test("uses Generic elsewhere") {
        assertTrue(detect(osId = Some("ubuntu")) == BrowserEnv.Generic)
      },
    ),
    suite("configFor")(
      test("launches the NixOS system browser without its sandbox") {
        val config = ChekhovSupport.configFor(BrowserEnv.NixOS, Some("/usr/bin/chromium"))
        assertTrue(
          config.executablePath.contains("/usr/bin/chromium"),
          config.channel.isEmpty,
          config.launchArgs.contains("--no-sandbox"),
          config.headless,
        )
      },
      test("uses the Chrome channel on GitHub Actions") {
        val config = ChekhovSupport.configFor(BrowserEnv.GitHubActions, Some("/usr/bin/google-chrome"))
        assertTrue(
          config.channel.contains("chrome"),
          config.executablePath.isEmpty,
          config.headless,
        )
      },
    ),
  )
