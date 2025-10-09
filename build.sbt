enablePlugins(PlayScala)

name := "webjars"

scalaVersion := "2.13.17"

libraryDependencies ++= Seq(
  ws,
  caffeine,
  guice,
  filters,
  "net.spy" % "spymemcached" % "2.12.3",
  "com.lihaoyi" %% "fastparse" % "3.1.1",
  "org.apache.commons" % "commons-compress" % "1.28.0",
  "org.eclipse.jgit" % "org.eclipse.jgit" % "7.4.0.202509020913-r",
  "org.bouncycastle" % "bcpg-jdk18on" % "1.82",
  "org.bouncycastle" % "bcutil-jdk18on" % "1.82",
  "com.outr" %% "hasher" % "1.2.3",
  "com.indoorvivants" %% "scala-uri" % "4.2.0",
  "net.ruippeixotog" %% "scala-scraper" % "3.2.0",
  "com.lumidion"  %%  "sonatype-central-client-requests"  %  "0.6.0",
  "org.webjars" %% "webjars-play" % "3.0.9",
  // brought in transatively via select2-bootstrap-5-theme
  //"org.webjars" % "bootstrap" % "5.3.3",
  "org.webjars" % "highlightjs" % "11.11.1",
  "org.webjars" % "jquery" % "3.7.1",
  "org.webjars" % "select2" % "4.0.13",
  "org.webjars.npm" % "select2-bootstrap-5-theme" % "1.3.0",
  "org.webjars.npm" % "jquery.typewatch" % "2.1.0",
  specs2 % Test
)

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-explaintypes",
  "-feature",
  "-Xcheckinit",
  "-Xfatal-warnings",
  "-Xlint:adapted-args",
  "-Xlint:constant",
  "-Xlint:delayedinit-select",
  "-Xlint:doc-detached",
  "-Xlint:inaccessible",
  "-Xlint:infer-any",
  "-Xlint:nullary-unit",
  "-Xlint:option-implicit",
  "-Xlint:package-object-classes",
  "-Xlint:poly-implicit-overload",
  "-Xlint:private-shadow",
  "-Xlint:stars-align",
  "-Xlint:type-parameter-shadow",
  "-Ywarn-dead-code",
  "-Ywarn-extra-implicit",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused:implicits",
  "-Ywarn-unused:locals",
  //"-Ywarn-unused:params", // disabled because Play routes needs a param to match a path
  "-Ywarn-unused:patvars",
  "-Ywarn-unused:privates",
)

pipelineStages := Seq(gzip, digest)

Compile / packageDoc / publishArtifact := false

Compile / doc / sources := Seq.empty
