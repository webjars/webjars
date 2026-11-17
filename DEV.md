# Dev Info

Run the web app in dev mode:

```
./sbta ~Test/runReload
```

This boots on a fresh Postgres database (Testcontainers, needs Docker) and
hydrates it by fetching `/all` from
`https://www.webjars.org` once at startup — always fresh, no cache blob to
keep in git. If the fetch fails the app still boots with an empty cache
(the home page shows the "Indexing artifacts" placeholder). The test app
never runs the live Maven Central refresh — that fan-out triggers 429s
against our prod IP.


Run the all tests:

1. `./sbta test`

Run a specific test, i.e.:
1. `testOnly webjars.DeployFailureSpec`

Run the real-browser JavaScript interaction tests:

```
./sbta 'Test / testOnly webjars.NewWebJarBrowserSpec webjars.ChekhovEnvSpec'
```

These tests use Chekhov with a system Chromium/Chrome plus Node and npm. Set
`WEBJARS_PW_CHROME` to override the browser executable. The pinned Playwright
driver is installed without downloading a browser; environments without the
required tools skip the real-browser scenario.


(todo: how do we avoid fully repopulating the maven central cache)
Run in prod mode:

1. Start a local Postgres server
    ```
    docker run -it -p5432:5432 -e POSTGRES_PASSWORD=webjars postgres:18.4
    ```
1. Set env vars:

    ```
    export DATABASE_URL='postgres://postgres:webjars@localhost:5432/postgres?sslmode=disable'
    ```

1. Apply the schema: `./sbta "runMain webjars.FlywayMigrate"`
1. Optionally load the WebJars from the live site: `./sbta "Test/runMain webjars.WebJarsBackfill"`
1. `./sbta run`

Schema changes go in a new `src/main/resources/db/migration/V<n>__name.sql`.
Never edit an applied one: Flyway checksums them and the release would fail.
Heroku's release phase (`release: bin/flyway-migrate` in the Procfile)
migrates while the old release still serves traffic, so a migration must work
with the code already deployed (add, then use; never drop what live code reads).
