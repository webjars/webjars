- Skills are authoritative reusable guidance, not content to copy into this file. Use the `sbt-mcp-webjars` `sbt-task` tool with command `extractSkillsJars` to extract the build's SkillsJars dependencies into `.kiro/skills/` (regenerated, gitignored, auto-discovered by Kiro). Read and follow every skill relevant to the task.

- For Scala 3 / ZIO implementation and review, follow the extracted `zen-of-scala` skill. For design principles, domain modeling, effects, and testing strategy, follow `zen-of-james`.

- Use the MCP server named `sbt-mcp-webjars` for ALL sbt interactions. Run commands/tasks through its `sbt-task` tool; use `list-tasks` to discover tasks/settings or get per-task help. Do not invoke `./sbt`, a shell, or a separate sbt client when the MCP tool is available. If the MCP server is unavailable, state that clearly; use a direct CLI fallback only when necessary and capture its output in `/tmp`.

- Run tests through `sbt-task` too. A failing `Test/testOnly <Spec>` or `testFull` response lists each failed test as `sbt-mcp test FAILED: <Spec> / <test>` with its assertion message, so there's no need for `./sbt --client` to see why a test failed. sbt 2 caches passing tests: a `testOnly` that finishes in about a second may have run nothing; use `testFull` for a real run.

- Use `sbt-mcp-webjars` for Scala/classpath symbol work: `glob-search` to find/list symbols, `inspect` for members/signatures, and `symbol-location` for source locations. Prefer these over text search, dependency-jar inspection, or guessed APIs whenever the question is about Scala symbols.

- This project uses sbt-reload; its instructions are at https://github.com/jamesward/sbt-reload/blob/main/README.md. Check it with `sbt-task` command `Test/reloadStatus`. If running, pause it with `Test/reloadPause` before edits and resume with `Test/reloadResume` afterward. Use `Test/reloadOutput` to read the latest compile/run output from a dev sbt instance.

## Agent tooling

- Follow the `zen-of-projects` Skill (extract with `./sbt extractSkillsJars` into the gitignored `.kiro/skills/`); this file records only project-specific facts and exceptions.
- MCP server `sbt-mcp-webjars` (sbt-mcp) listens on `http://127.0.0.1:5055/`. Kiro uses the HTTP entry in `.kiro/settings/mcp.json`; start sbt first. Claude Code uses `.mcp.json`, which runs `.claude/sbt-mcp-stdio.sh` (approved in `.claude/settings.json`). That stdio bridge starts a foreground sbt in cloud sessions (`CLAUDE_CODE_REMOTE=true`), and locally only connects to an sbt you already started. Its tools are deferred: load them with ToolSearch (search `sbt-mcp-webjars`). Diagnostics go to `/tmp/sbt-mcp-stdio.log` and `/tmp/sbt-mcp-server.log`.
- Maintenance routine: `.factory/MAINTENANCE.md` (weekly), following the `zen-of-projects` Skill.
