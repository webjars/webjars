WebJars
=======

[![Join the chat at https://gitter.im/webjars/webjars](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/webjars/webjars?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

This is source for the http://www.webjars.org website.  This is also the place to [file requests](https://github.com/webjars/webjars/issues) for new WebJars.

For more information about WebJars visit the website: http://www.webjars.org

[![Travis CI Build Status](https://travis-ci.org/webjars/webjars.svg?branch=master)](https://travis-ci.org/webjars/webjars)


Development Info
----------------

Run the web app:

1. Start a local memcache server
1. `./sbt ~run`

> Note: Deploying WebJars to BinTray / Maven Central requires a bunch of stuff.  In the future that will be documented but for now, deployment won't work in plain development environments.

Run the all tests:

1. `./sbt test`

Run a specific test, i.e.:
1. `testOnly utils.LicenseDetectorSpec -- ex "detect the license"`

```
export OSS_GPG_PASS=asdf
export OSS_GPG_KEY=$(gpg --no-tty --pinentry-mode loopback --passphrase "$OSS_GPG_PASS" --export-secret-keys | base64 -w 0)
```