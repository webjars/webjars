package models

case class WebJar(artifactId: String, name: String, sourceUrl: String, versions: Seq[WebJarVersion])

case class WebJarVersion(number: String, files: String)

object WebJars {
  
  val all: Seq[WebJar] = Seq(
    WebJar("jquery", "jQuery", "http://github.com/webjars/jquery", Seq(
      WebJarVersion("1.8.2", """
        META-INF/resources/webjars/jquery/1.8.2/jquery.min.js
        META-INF/resources/webjars/jquery/1.8.2/jquery.js"""
      ),
      WebJarVersion("1.8.1", """
        META-INF/resources/jquery/1.8.1/jquery.min.js
        META-INF/resources/jquery/1.8.1/jquery.js"""
      )
    )),
    WebJar("bootstrap", "Bootstrap", "http://github.com/webjars/bootstrap", Seq(
      WebJarVersion("2.1.1", """
       META-INF/resources/webjars/bootstrap/2.1.1/js/bootstrap.js
       META-INF/resources/webjars/bootstrap/2.1.1/js/bootstrap.min.js
       META-INF/resources/webjars/bootstrap/2.1.1/img/glyphicons-halflings-white.png
       META-INF/resources/webjars/bootstrap/2.1.1/img/glyphicons-halflings.png
       META-INF/resources/webjars/bootstrap/2.1.1/css/bootstrap.min.css
       META-INF/resources/webjars/bootstrap/2.1.1/css/bootstrap.css
       META-INF/resources/webjars/bootstrap/2.1.1/css/bootstrap-responsive.css
       META-INF/resources/webjars/bootstrap/2.1.1/css/bootstrap-responsive.min.css"""
      )
    )),
    WebJar("lodash", "Lo-Dash", "http://github.com/webjars/lodash", Seq(
      WebJarVersion("0.7.0", """
        META-INF/resources/webjars/lodash/0.7.0/lodash.min.js
        META-INF/resources/webjars/lodash/0.7.0/lodash.js"""
      )
    )),
    WebJar("hogan.js", "Hogan.js", "http://github.com/webjars/hogan.js", Seq(
      WebJarVersion("2.0.0", """
        META-INF/resources/webjars/hogan.js/2.0.0/hogan.min.js
        META-INF/resources/webjars/hogan.js/2.0.0/hogan.js"""
      )
    )),
    WebJar("highcharts", "Highcharts", "http://github.com/webjars/highcharts", Seq(
      WebJarVersion("2.3.2", """
        META-INF/resources/webjars/highcharts/2.3.2/highcharts.js"""
      )
    )),
    WebJar("highstock", "Highstock", "http://github.com/webjars/highstock", Seq(
      WebJarVersion("1.2.2", """
        META-INF/resources/webjars/highstock/1.2.2/highstock.js"""
      )
    )),
    WebJar("select2", "Select2", "http://github.com/webjars/select2", Seq(
      WebJarVersion("3.2", """
        META-INF/resources/webjars/select2/3.2/select2.png
        META-INF/resources/webjars/select2/3.2/select2x2.png
        META-INF/resources/webjars/select2/3.2/select2.css
        META-INF/resources/webjars/select2/3.2/spinner.gif
        META-INF/resources/webjars/select2/3.2/select2.min.js
        META-INF/resources/webjars/select2/3.2/select2.js"""
      )
    ))
  )
}