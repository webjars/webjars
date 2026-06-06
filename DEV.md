# Dev Info

Run the web app in dev mode:

```
./sbt ~reStartTest
```

This hydrates the local Valkey by fetching `/all` from
`https://www.webjars.org` once at startup — always fresh, no cache blob to
keep in git. If the fetch fails the app still boots with an empty cache
(the home page shows the "Indexing artifacts" placeholder). The test app
never runs the live Maven Central refresh — that fan-out triggers 429s
against our prod IP.


Run the all tests:

1. `./sbt test`

Run a specific test, i.e.:
1. `testOnly webjars.DeployFailureSpec`


(todo: how do we avoid fully repopulating the maven central cache)
Run in prod mode:

1. Start a local valkey server
    ```
    docker run -it -p6379:6379 valkey/valkey:8.1
    ```
1. Set env vars:

    ```
    export REDIS_URL=redis://localhost:6379
    ```

1. `./sbt run`
