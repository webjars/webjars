# Dev Info

Run the web app in dev mode:

```
./sbt ~reStartTest
```


Run the all tests:

1. `./sbt test`

Run a specific test, i.e.:
1. `testOnly utils.LicenseDetectorSpec -- ex "detect the license"`


Run in prod mode:

1. Start a local valkey server
    ```
    docker run -it -p6379:6379 valkey/valkey:8.1
    ```
1. Set env vars:

    ```
    export MAVEN_CENTRAL_LIMIT=5
    export REDIS_URL=redis://localhost:6379
    ```

1. `./sbt run`
