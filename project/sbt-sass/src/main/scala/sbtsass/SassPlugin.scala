package sbtsass

import sbt._
import sbt.Keys._

import de.larsgrefer.sass.embedded.SassCompilerFactory

import java.net.URLClassLoader

/**
 * Compiles SCSS to CSS at build time using dart-sass-embedded (no Node).
 *
 * Adds a hidden `sass` Ivy configuration. WebJar dependencies declared with
 * the `Sass` scope are made available to SCSS via [[WebJarsScssImporter]],
 * so `@import "bootstrap/scss/functions";` resolves against the bootstrap
 * webjar JAR on the build classpath (trying SCSS suffix conventions like
 * `_functions.scss`).
 *
 * Generated CSS lands in Compile / resourceManaged / "public", which the
 * runtime serves under /assets/.
 */
object SassPlugin extends AutoPlugin {
  // Auto-enabled on every JVM project. The `% Set(...)` implicit comes
  // from WebJarsPlugin's autoImport (which is also auto-enabled).
  override def trigger  = allRequirements
  override def requires = sbt.plugins.JvmPlugin

  object autoImport {
    val Sass = config("sass").hide

    val sassSource  = settingKey[File]("Root SCSS source dir")
    val sassTarget  = settingKey[File]("Output dir for compiled CSS")
    val sassCompile = taskKey[Seq[File]]("Compile SCSS to CSS")
  }
  import autoImport._

  override def projectConfigurations: Seq[Configuration] = Seq(Sass)

  override def projectSettings: Seq[Setting[_]] =
    inConfig(Sass)(Defaults.configSettings) ++ Seq(
      ivyConfigurations += Sass,
      sassSource := (Compile / sourceDirectory).value / "scss",
      sassTarget := (Compile / resourceManaged).value / "public",

      sassCompile := {
        val log    = streams.value.log
        val srcDir = sassSource.value
        val outDir = sassTarget.value
        val cpJars = (Sass / dependencyClasspath).value.files

        if (!srcDir.exists) {
          log.info(s"[sass] no SCSS sources at $srcDir, skipping")
          Seq.empty[File]
        } else {
          IO.createDirectory(outDir)
          val loader = new URLClassLoader(cpJars.map(_.toURI.toURL).toArray, getClass.getClassLoader)
          val compiler = SassCompilerFactory.bundled()
          try {
            compiler.registerImporter(new WebJarsScssImporter(loader))
            val entries = (srcDir ** "*.scss").get.filterNot(_.getName.startsWith("_"))
            entries.map { src =>
              val rel = IO.relativize(srcDir, src).getOrElse(src.getName)
              val out = outDir / rel.stripSuffix(".scss").concat(".css")
              IO.createDirectory(out.getParentFile)
              val result = compiler.compileFile(src)
              IO.write(out, result.getCss)
              log.info(s"[sass] $src -> $out")
              out
            }
          } finally {
            compiler.close()
          }
        }
      },

      Compile / resourceGenerators += sassCompile.taskValue,
    )
}
