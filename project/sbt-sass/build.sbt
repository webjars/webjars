sbtPlugin    := true
name         := "sbt-sass"
organization := "webjars"
version      := "0.1.0-SNAPSHOT"

// sbt 1.x plugins run on Scala 2.12
scalaVersion := "2.12.20"

libraryDependencies ++= Seq(
  "de.larsgrefer.sass" % "sass-embedded-host"    % "4.4.0",
  "de.larsgrefer.sass" % "sass-embedded-bundled" % "4.4.0",
  // Required by dart-sass-java's bundled `WebjarsImporter` — not transitive
  // through `sass-embedded-host`'s pom even though the class hard-references it.
  "org.webjars"        % "webjars-locator-core"  % "0.59",
)
