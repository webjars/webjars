WebJars
=======

[![.github/workflows/test.yml](https://github.com/webjars/webjars/actions/workflows/test.yml/badge.svg)](https://github.com/webjars/webjars/actions/workflows/test.yml)

This is source for the http://www.webjars.org website.  This is also the place to [file requests](https://github.com/webjars/webjars/issues) for new WebJars.

For more information about WebJars visit the website: http://www.webjars.org


Development Info
----------------

Run the web app:

1. Start a local valkey server
    ```
    docker run -it -p6379:6379 valkey/valkey:8.1
    ```
1. `./sbt ~run`

Run the all tests:

1. `./sbt test`

Run a specific test, i.e.:
1. `testOnly utils.LicenseDetectorSpec -- ex "detect the license"`

```
export OSS_GPG_PASS=asdf
export OSS_GPG_KEY=$(gpg --no-tty --pinentry-mode loopback --passphrase "$OSS_GPG_PASS" --export-secret-keys | base64 -w 0)
```
