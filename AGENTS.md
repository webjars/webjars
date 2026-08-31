- Skills are authoritative reusable guidance, not content to copy into this file. Use the `sbt-mcp-webjars` `sbt-task` tool with command `extractSkillsJars` to extract the build's SkillsJars dependencies into `.kiro/skills/` (regenerated, gitignored, auto-discovered by Kiro). Read and follow every skill relevant to the task.

- For Scala 3 / ZIO implementation and review, follow the extracted `zen-of-scala` skill. For design principles, domain modeling, effects, and testing strategy, follow `zen-of-james`.

- Use the MCP server named `sbt-mcp-webjars` for ALL sbt interactions. Run commands/tasks through its `sbt-task` tool; use `list-tasks` to discover tasks/settings or get per-task help. Do not invoke `./sbt`, a shell, or a separate sbt client when the MCP tool is available. If the MCP server is unavailable, state that clearly; use a direct CLI fallback only when necessary and capture its output in `/tmp`.

- Use `sbt-mcp-webjars` for Scala/classpath symbol work: `glob-search` to find/list symbols, `inspect` for members/signatures, and `symbol-location` for source locations. Prefer these over text search, dependency-jar inspection, or guessed APIs whenever the question is about Scala symbols.

- This project uses sbt-reload; its instructions are at https://github.com/jamesward/sbt-reload/blob/main/README.md. Check it with `sbt-task` command `Test/reloadStatus`. If running, pause it with `Test/reloadPause` before edits and resume with `Test/reloadResume` afterward. Use `Test/reloadOutput` to read the latest compile/run output from a dev sbt instance.
