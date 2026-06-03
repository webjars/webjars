// In-repo plugin subprojects under project/ (composed via meta-build
// dependency). Their classes become available to build.sbt without
// publishLocal. Paths are relative to project/.
val sbtSass    = RootProject(file("sbt-sass"))

dependsOn(sbtSass)

addSbtPlugin("io.spray" % "sbt-revolver" % "0.10.0")

addSbtPlugin("com.github.sbt" % "sbt-native-packager" % "1.11.7")

addSbtPlugin("org.webjars" % "sbt-webjars" % "0.0.2")
