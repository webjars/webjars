package webjars.views.documentations

// Each documentation page is a simple function returning static HTML content.
// These are kept in a single file since they're all short static content.

object Usage:
  def apply(): String = """<h2>WebJars work with most JVM-based containers and web frameworks.</h2>

<h3>Using a WebJar requires:</h3>

<ol>
    <li>The WebJar needs to be a dependency of your application</li>
    <li>The WebJar needs to be in your application's running CLASSPATH</li>
    <li>Your container, web framework, or application needs to serve static assets from Jar files</li>
</ol>

<h3>Public CDN via <a href="http://www.jsdelivr.com" target="_blank">jsDelivr</a></h3>

<p>
    All of the WebJar contents are available on the public <a href="http://www.jsdelivr.com" target="_blank">jsDelivr</a> CDN.
    Just prefix <code>//cdn.jsdelivr.net/webjars/{groupId}</code> in front of your static asset URLs.  For instance, if using
    the <code>org.webjars : jquery</code> WebJar and your local URL to <code>jquery.js</code> is <code>/webjars/jquery/2.1.0/jquery.js</code>
    then the CDN URL would be: <code>//cdn.jsdelivr.net/webjars/org.webjars/jquery/2.1.0/jquery.js</code>
</p>"""

object PlayFramework:
  def apply(): String = """<h2>Instructions for Play 2.6 (<a href="https://github.com/webjars/sample-play2" target="_blank" rel="noopener noreferrer">Sample Source</a>)</h2>

<p>
    WebJars can be added as dependencies to an app by simply adding them to the <code>build.sbt</code> file like:
</p>
<pre><code>libraryDependencies += "org.webjars" % "bootstrap" % "3.1.1-2"</code></pre>

<p>
    Play automatically extracts the WebJar contents and makes them available via the <code>Assets</code> controller.
</p>"""

object Xitrum:
  def apply(): String = """<h2>Instructions for Xitrum</h2>

<p>
    Xitrum can serve static files from classpath, which includes WebJars.
    See <a href="http://xitrum-framework.github.io/guide/static.html" target="_blank" rel="noopener noreferrer">Xitrum documentation</a>.
</p>"""

object Servlet3:
  def apply(): String = """<h2>Instructions for Servlet 3</h2>

<p>
    With any Servlet 3 compatible container, the WebJars that are in the <code>WEB-INF/lib</code> directory are automatically
    made available as static resources.  This works because anything in a <code>META-INF/resources</code> directory in a JAR
    in <code>WEB-INF/lib</code> is automatically exposed as a static resource.
</p>

<h3>Maven Example (<a href="https://github.com/webjars/sample-jetty_war" target="_blank" rel="noopener noreferrer">example app</a>)</h3>

<p>First add a WebJar as a dependency of your application in the <code>pom.xml</code> file, like:</p>
<pre><code>&lt;dependencies&gt;
    &lt;dependency&gt;
        &lt;groupId&gt;org.webjars&lt;/groupId&gt;
        &lt;artifactId&gt;bootstrap&lt;/artifactId&gt;
        &lt;version&gt;3.1.0&lt;/version&gt;
    &lt;/dependency&gt;
&lt;/dependencies&gt;</code></pre>

<p>Then simply reference the resource like:</p>
<pre><code>&lt;link rel='stylesheet' href='webjars/bootstrap/3.1.0/css/bootstrap.min.css'&gt;</code></pre>"""

object Servlet2:
  def apply(): String = """<h2>Instructions for Servlet 2</h2>

<p>
    WebjarsServlet allows Webjars resources to be referenced in legacy apps that are still running on Servlet containers that
    are not compliant with the Servlet 3 specification.
</p>

<h3>Usage</h3>
<p>Register the webjars-servlet-2.x Maven dependency and WebjarsServlet in your web.xml.</p>"""

object Jsf:
  def apply(): String = """<h2>Instructions for JSF</h2>

<p>
    With JSF, the WebJars that are in the <code>WEB-INF/lib</code> directory are automatically made available as resource
    libraries. This works because WebJars are compatible with the JSF resource identifier format.
</p>

<h3>Maven Example (<a href="https://github.com/arend-von-reinersdorff/samplejsfwithwebjars" target="_blank" rel="noopener noreferrer">example app</a>)</h3>

<p>First add a WebJar as a dependency, then reference the resource like:</p>
<pre><code>&lt;h:outputStylesheet library="webjars" name="bootstrap/3.1.0/css/bootstrap.min-jsf.css" /&gt;</code></pre>"""

object Grails:
  def apply(): String = """<h2>Instructions for Grails (<a href="https://github.com/webjars/sample-grails" target="_blank" rel="noopener noreferrer">example app</a>)</h2>

<p>
    Grails manages static resources with the resources plugin. The
    <a href="https://github.com/groovydev/modules-manager-grails-plugin" target="_blank" rel="noopener noreferrer">
        Grails Modules Manager plugin
    </a> allows dependencies on web libraries to be declared in the Grails build configuration file.
</p>"""

object Dropwizard:
  def apply(): String = """<h2>Instructions for Dropwizard (<a href="https://github.com/webjars/sample-dropwizard" target="_blank" rel="noopener noreferrer">example app</a>)</h2>

<p>
    With Dropwizard you can easily expose WebJars through the <code>AssetsBundle</code>.
</p>
<pre><code>addBundle(new AssetsBundle("/META-INF/resources/webjars", 0, "/webjars"));</code></pre>

<p>Now you can reference a WebJar asset like:</p>
<pre><code>&lt;link rel='stylesheet' href='/webjars/bootstrap/3.1.0/css/bootstrap.min.css'&gt;</code></pre>"""

object SpringBoot:
  def apply(): String = """<h2>Instructions for Spring Boot (<a href="https://github.com/webjars/sample-spring_boot" target="_blank" rel="noopener noreferrer">example app</a>)</h2>

<p>
    Spring Boot automatically configures Spring to map requests for <code>/webjars</code> to the <code>/META-INF/resources/webjars</code>
    directory of all the JARs in the CLASSPATH.
</p>

<h3>Maven Example</h3>

<p>First add a WebJar as a dependency, then reference a WebJar asset like:</p>
<pre><code>&lt;link rel='stylesheet' href='/webjars/bootstrap/3.1.0/css/bootstrap.min.css'&gt;</code></pre>"""

object SpringMvc:
  def apply(): String = """<h2>Instructions for Spring MVC</h2>

<p>
    Spring MVC makes it easy to expose static assets in JAR files using <code>ResourceHandlers</code>.
</p>

<h3>Maven Example (<a href="https://github.com/webjars/sample-jetty_war" target="_blank" rel="noopener noreferrer">example app</a>)</h3>

<p>Configure Spring to map requests for <code>/webjars</code> to the <code>/META-INF/resources/webjars</code> directory.</p>"""

object Tapestry:
  def apply(): String = """<h2>Instructions for Apache Tapestry (<a href="https://github.com/webjars/sample-tapestry" target="_blank" rel="noopener noreferrer">example app</a>)</h2>

<p>
    Apache Tapestry makes it easy to expose static assets in JAR files using <code>contributeClasspathAssetAliasManager</code>.
</p>"""

object Wicket:
  def apply(): String = """<h2>Instructions for Apache Wicket (<a href="https://github.com/webjars/sample-wicket" target="_blank" rel="noopener noreferrer">example app</a>)</h2>

<p>
    The Wicket integration of Webjars uses a special <code>IResourceFinder</code> implementation to map Webjars resources.
</p>"""

object Pippo:
  def apply(): String = """<h2>Instructions for Pippo</h2>

<p>The <a href="http://www.pippo.ro" target="_blank" rel="noopener noreferrer">Pippo</a> integration of Webjars is pretty straightforward.</p>"""

object Ring:
  def apply(): String = """<h2>Instructions for Ring (<a href="https://github.com/webjars/sample-clojure" target="_blank" rel="noopener noreferrer">example app</a>)</h2>

<p>Ring makes it easy to expose WebJars through the <code>wrap-resource</code> function.</p>"""

object Dandelion:
  def apply(): String = """<h2>Instructions for Dandelion</h2>

<p>
    <a href="http://dandelion.github.io/" target="_blank" rel="noopener noreferrer">Dandelion</a> provides a WebJars integration
    via a dedicated locator for WebJars.
</p>"""

object Vertx:
  def apply(): String = """<h2>Instructions for Vert.x Web</h2>

<p>
    The Vert.x Web <code><a href="https://vertx.io/docs/vertx-web/java/#_serving_static_resources" target="_blank" rel="noopener noreferrer">StaticHandler</a></code>
    can resolve file paths from both the filesystem and the classpath.
</p>"""

object Quarkus:
  def apply(): String = """<h2>Instructions for Quarkus</h2>

<p>
    Quarkus support the use of WebJars by default and also adds a
    <a href="https://quarkus.io/guides/http-reference#webjar-locator-support" target="_blank" rel="noopener noreferrer">WebJar Locator</a>
    to reference WebJars without the version.
</p>"""

object Ktor:
  def apply(): String = """<h2>Instructions for Ktor (<a href="https://github.com/webjars/sample-ktor" target="_blank" rel="noopener noreferrer">Sample Source</a>)</h2>

<p>
    Add the <a href="https://github.com/webjars/webjars-locator-lite" target="_blank" rel="noopener noreferrer">webjars-locator-lite</a>
    dependency and your WebJars, then define a routing path with the locator.
</p>"""
