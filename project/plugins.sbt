// In-repo plugin subprojects under project/ (composed via meta-build
// dependency). Their classes become available to build.sbt without
// publishLocal. Paths are relative to project/.
val sbtWebjars = RootProject(file("sbt-webjars"))
val sbtSass    = RootProject(file("sbt-sass"))

dependsOn(sbtWebjars, sbtSass)

addSbtPlugin("io.spray" % "sbt-revolver" % "0.10.0")
