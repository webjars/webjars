# Maintenance Routine

If there are other open PRs for this work, update that PR instead of creating a new one.

Whenever a step says to stop, or the run can't finish: revert your uncommitted edits
(`git checkout -- .`), don't push a branch or open a PR, and end with a report that quotes what
failed. A cloud session's Stop hook asks you to commit uncommitted changes; don't commit unvalidated
work to satisfy it. Don't send push notifications: the routine emails its result, and your final
reply is the report.

0. Load the project's MCP tools before anything else. `AGENTS.md` names the MCP server:
   `sbt-mcp-<project>` for sbt projects, `javadocs` for Maven and Gradle projects. In Claude Code
   these tools are deferred, so load them with ToolSearch (search for the server name). They include
   the javadocs.dev tools such as `get_latest_version`, and for sbt also `sbt-task`. Use them for
   the rest of the run, and fall back to the build tool's launcher and `curl` only when they are
   unavailable. Say which one you used.
1. Update the Skills dependency, `com.jamesward:skills`, to its latest release. Where it's pinned:
   - sbt: `"com.jamesward" % "skills" % "<version>" % Skills` in `build.sbt`
   - Maven: a dependency of the `com.skillsjars:maven-plugin` plugin in `pom.xml`
   - Gradle: `skill("com.jamesward:skills:<version>")` in `build.gradle.kts` or `gradle/libs.versions.toml`

   Some projects also pin it in an `example/` build. List every pin with:

   ```bash
   grep -rnE 'com\.jamesward.{0,20}skills|<artifactId>skills</artifactId>' --include='*.sbt' --include=pom.xml --include='*.gradle' --include='*.gradle.kts' --include='*.toml' . | grep -v -e /target/ -e /build/ -e /src/sbt-test/
   ```

   Get the latest release with `get_latest_version` (group `com.jamesward`, artifact `skills`). Without
   MCP, ask Maven Central itself, not a mirror (mirrors lag new releases):

   ```bash
   curl -fsS --retry 5 --retry-delay 10 --retry-all-errors https://repo.maven.apache.org/maven2/com/jamesward/skills/maven-metadata.xml | sed -n 's:.*<release>\(.*\)</release>.*:\1:p'
   ```

   Maven Central can rate-limit cloud sessions (HTTP 429); the retries cover that. Set every pin
   to the version it prints.
2. Extract the Skills to `.kiro/skills/`:
   - sbt: `extractSkillsJars` through the sbt-mcp `sbt-task` tool, or `./sbt extractSkillsJars`.
     After `build.sbt` changes, sbt reloads and restarts the sbt-mcp server. The first `sbt-task`
     call afterwards can report a lost connection; run it again rather than switching to `./sbt`.
   - Maven: `./mvnw -q skillsjars:extract`
   - Gradle: `./gradlew extractSkillsJars`

   `.kiro/skills/` is gitignored, so it doesn't exist until this runs. If the build can't download
   artifacts, quote the exact error; don't guess at a cause such as a rate limit. A "Not found" for a
   version released in the last day means it hasn't reached every mirror yet: pin the newest version
   that does resolve, continue, and say so in the report. For any other download error (for example
   HTTP 429 or a proxy 403), stop and report it instead of changing resolvers.
3. Read `.kiro/skills/*zen-of-projects*/SKILL.md` and follow its "Maintenance Routine" section, using
   `AGENTS.md` for this project's commands and documented exceptions. While an unreleased version of
   the Skill is being tested, `.factory/skills/zen-of-projects/SKILL.md` exists. Read that file
   instead, and don't delete it.
