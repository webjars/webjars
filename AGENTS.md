- Skills are authoritative reusable guidance, not content to copy into this file. Use the `sbta` `sbt-task` tool with command `extractSkillsJars` to extract the build's SkillsJars dependencies into `.kiro/skills/` (regenerated, gitignored, auto-discovered by Kiro). Read and follow every skill relevant to the task.

- For Scala 3 / ZIO implementation and review, follow the extracted `zen-of-scala` skill. For design principles, domain modeling, effects, and testing strategy, follow `zen-of-james`.

- Use the MCP server named `sbta` for ALL sbt interactions. Run commands/tasks through its `sbt-task` tool; use `list-tasks` to discover tasks/settings or get per-task help. Do not invoke `./sbta`, a shell, or a separate sbt client when the MCP tool is available. If the MCP server is unavailable, state that clearly; use a direct CLI fallback only when necessary and capture its output in `/tmp`.

- Run tests through `sbt-task` too. A failing `Test/testOnly <Spec>` or `testFull` response lists each failed test as `sbt-mcp test FAILED: <Spec> / <test>` with its assertion message, so there's no need for `./sbta --client` to see why a test failed. sbt 2 caches passing tests: a `testOnly` that finishes in about a second may have run nothing; use `testFull` for a real run.

- Use `sbta` for Scala/classpath symbol work: `glob-search` to find/list symbols, `inspect` for members/signatures, and `symbol-location` for source locations. Prefer these over text search, dependency-jar inspection, or guessed APIs whenever the question is about Scala symbols.

- This project uses sbt-reload; its instructions are at https://github.com/jamesward/sbt-reload/blob/main/README.md. Check it with `sbt-task` command `Test/reloadStatus`. If running, pause it with `Test/reloadPause` before edits and resume with `Test/reloadResume` afterward. Use `Test/reloadOutput` to read the latest compile/run output from a dev sbt instance.

## Agent tooling

- Follow the `zen-of-projects` Skill (extract with `./sbta extractSkillsJars` into the gitignored `.kiro/skills/`); this file records only project-specific facts and exceptions.
- MCP server `sbta` (this build's `./sbta --mcp`, configured in `.mcp.json` and `.kiro/settings/mcp.json`, approved in `.claude/settings.json`): use it for all sbt work: `compile`, `test` (`full` for all), `run`, `check` after editing Scala 3 sources, `glob-search` / `inspect` / `symbol-location` for symbols, `sbt-task` for any other command. Its tools are deferred in Claude Code: load them with ToolSearch (search `sbta`). If it's unavailable, say so and use `./sbta <command>`. Logs: `project/target/sbta/mcp.log`.
- Maintenance routine: `.factory/MAINTENANCE.md` (weekly), following the `zen-of-projects` Skill.
