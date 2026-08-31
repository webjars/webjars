package webjars

import chekhov.*
import chekhov.driver.PlaywrightDriver
import webjars.generated.WebJars as Gen
import webjars.models.{WebJar, WebJarVersion}
import webjars.routes.StaticAssets
import webjars.views.partials.{NewWebJarModal, WebJarList}
import zio.*
import zio.http.*
import zio.http.template2.*
import zio.json.*
import zio.test.*
import zio.test.TestAspect.*

object NewWebJarBrowserSpec extends ZIOSpecDefault:

  private val instrumentation =
    """window.__uiErrors=[];
      |addEventListener('error',function(e){window.__uiErrors.push(String((e&&e.message)||e));});
      |addEventListener('unhandledrejection',function(e){window.__uiErrors.push('rejection:'+String(e&&e.reason));});
      |window.__existsCalls=0;
      |window.__searchCalls=0;
      |window.__popularCalls=0;
      |window.__realFetch=window.fetch.bind(window);
      |window.fetch=function(input,init){
      |  var url=typeof input==='string'?input:input.url;
      |  if(url.indexOf('/exists?')===0)window.__existsCalls++;
      |  if(url.indexOf('/search?')===0)window.__searchCalls++;
      |  if(url==='/popular')window.__popularCalls++;
      |  return window.__realFetch(input,init);
      |};
      |window.__snapshot=function(){
      |  var alert=document.getElementById('deployError');
      |  return JSON.stringify({
      |    npm:document.getElementById('newWebJarTypeNpm').checked,
      |    classic:document.getElementById('newWebJarTypeClassic').checked,
      |    disabled:document.getElementById('newWebJarName').disabled,
      |    name:document.getElementById('newWebJarName').value,
      |    calls:window.__existsCalls,
      |    searchCalls:window.__searchCalls,
      |    popularCalls:window.__popularCalls,
      |    jqueryPresent:typeof window.jQuery!=='undefined',
      |    listTitle:document.getElementById('listTitle').textContent,
      |    listText:document.getElementById('webJarList').textContent,
      |    buildInstructions:(document.querySelector('.build-instructions pre')||{}).textContent||'',
      |    versionDisabled:document.getElementById('newWebJarVersion').disabled,
      |    versionValues:Array.from(document.getElementById('newWebJarVersion').options).map(function(option){return option.value;}),
      |    alertHidden:alert.classList.contains('d-none'),
      |    alertTitle:document.getElementById('deployErrorTitle').textContent,
      |    alertMessage:document.getElementById('deployErrorMessage').textContent,
      |    alertWarning:alert.classList.contains('alert-warning'),
      |    deployLog:document.getElementById('deployLog').textContent,
      |    trackingHref:document.getElementById('deployErrorTrackingUrl').getAttribute('href')||'',
      |    errors:window.__uiErrors
      |  });
      |};
      |class FakeEventSource{
      |  constructor(url){this.url=url;this.listeners={};window.__eventSource=this;}
      |  addEventListener(type,listener){(this.listeners[type]||(this.listeners[type]=[])).push(listener);}
      |  emit(type,data){(this.listeners[type]||[]).forEach(function(listener){listener({data:data});});}
      |  close(){}
      |}
      |window.EventSource=FakeEventSource;
      |""".stripMargin

  private def listHtml(artifactId: String, name: String): String =
    WebJarList(Left(Seq(
      WebJar(
        "org.webjars.npm",
        artifactId,
        name,
        s"https://example.test/$artifactId",
        Seq(WebJarVersion("1.0.0", Some(3))),
      )
    ))).render

  private val initialListHtml = listHtml("initial", "Initial")
  private val searchListHtml  = listHtml("react", "React")
  private val popularListHtml = listHtml("bootstrap", "Bootstrap")

  private def pageHtml: String =
    val bootstrap = Gen.localUrl(Gen.Artifact.bootstrap, "dist/js/bootstrap.bundle.min.js")
    s"""<!DOCTYPE html><html lang="en"><head><meta charset="utf-8">
       |<script>$instrumentation</script><script src="$bootstrap"></script>
       |</head><body>
       |<input id="search" type="text"><button id="clearSearch" type="button">Clear</button>
       |<input type="checkbox" name="search_catalog[]" value="org.webjars.npm" checked>
       |<input type="checkbox" name="search_catalog[]" value="org.webjars" checked>
       |<div id="buildtoolselect">
       |  <input type="radio" name="buildtool" id="sbt" value="sbt" checked><label for="sbt">SBT</label>
       |  <input type="radio" name="buildtool" id="maven" value="maven"><label for="maven">Maven</label>
       |</div>
       |<h2 id="listTitle">Popular WebJars</h2><div id="webJarList">$initialListHtml</div>
       |<button id="classicLaunch" type="button" data-bs-toggle="modal" data-bs-target="#newWebJarModal" data-webjar-type="classic" data-artifact-id="jquery" data-name="jQuery">Classic +</button>
       |<button id="npmLaunch" type="button" data-bs-toggle="modal" data-bs-target="#newWebJarModal" data-webjar-type="npm" data-artifact-id="scope__package" data-name="@scope/package">NPM +</button>
       |${NewWebJarModal().render}
       |<script src="/assets/javascripts/index.js"></script>
       |</body></html>""".stripMargin

  private val routes: Routes[Any, Nothing] =
    Routes(
      Method.GET / "" -> handler(Response.html(Dom.raw(pageHtml))),
      Method.GET / "search" -> handler(Response.html(Dom.raw(searchListHtml))),
      Method.GET / "popular" -> handler(Response.html(Dom.raw(popularListHtml))),
      Method.GET / "exists" -> handler(
        Response(
          Status.Ok,
          Headers(Header.ContentType(MediaType.application.json).untyped),
          Body.fromString("""{"deployable":true,"versions":[],"error":null}"""),
        )
      ),
      Method.GET / "versions" -> handler(
        Response(
          Status.Ok,
          Headers(Header.ContentType(MediaType.application.json).untyped),
          Body.fromString("""["2.0.0","1.0.0"]"""),
        )
      ),
    ) ++ StaticAssets.routes ++ TestStaticAssets.routes

  private final case class SerializedString(s: String) derives JsonDecoder

  private final case class UiState(
      npm: Boolean,
      classic: Boolean,
      disabled: Boolean,
      name: String,
      calls: Int,
      searchCalls: Int,
      popularCalls: Int,
      jqueryPresent: Boolean,
      listTitle: String,
      listText: String,
      buildInstructions: String,
      versionDisabled: Boolean,
      versionValues: List[String],
      alertHidden: Boolean,
      alertTitle: String,
      alertMessage: String,
      alertWarning: Boolean,
      deployLog: String,
      trackingHref: String,
      errors: List[String],
  ) derives JsonDecoder

  private def evalString(page: Page, javascript: String): IO[Throwable, String] =
    page.evaluate(javascript, isFunction = true).flatMap: raw =>
      ZIO.fromEither(raw.fromJson[SerializedString])
        .mapBoth(error => RuntimeException(s"evaluate decode failed: $error (raw=$raw)"), _.s)

  private def state(page: Page): IO[Throwable, UiState] =
    evalString(page, "() => window.__snapshot()").flatMap: json =>
      ZIO.fromEither(json.fromJson[UiState])
        .mapError(error => RuntimeException(s"state decode failed: $error ($json)"))

  private def untilReady(page: Page, tries: Int): IO[Throwable, Unit] =
    evalString(page, "() => typeof checkPackageName === 'function' ? 'ready' : 'waiting'").flatMap:
      case "ready" => ZIO.unit
      case status if tries > 0 => ZIO.sleep(100.millis) *> untilReady(page, tries - 1)
      case status => ZIO.fail(RuntimeException(s"browser fixture did not become ready: $status"))

  private val program: ZIO[Page & Server, Throwable, TestResult] =
    for
      page          <- Chekhov.page
      port          <- Server.install(routes)
      _             <- page.goto(s"http://localhost:$port/")
      _             <- untilReady(page, 60)
      initial       <- state(page)
      _             <- page.evaluate(
                         "() => { var s=document.getElementById('search'); s.value='react'; s.dispatchEvent(new Event('input',{bubbles:true})); return 'searched'; }",
                         isFunction = true,
                       )
      _             <- ZIO.sleep(300.millis)
      searchLoaded  <- state(page)
      _             <- page.click("#clearSearch")
      _             <- ZIO.sleep(300.millis)
      popularLoaded <- state(page)
      _             <- page.click("label[for='maven']")
      mavenSelected <- state(page)
      _             <- page.click("label[for='newWebJarTypeNpm']")
      npmLabel      <- state(page)
      _             <- page.click("#classicLaunch")
      _             <- ZIO.sleep(500.millis)
      classicLaunch <- state(page)
      _             <- page.click("#newWebJarModal .btn-close")
      _             <- ZIO.sleep(500.millis)
      _             <- page.click("#npmLaunch")
      _             <- ZIO.sleep(500.millis)
      npmLaunch     <- state(page)
      emptyNames    <- evalString(
                         page,
                         "() => { window.__existsCalls=0; checkPackageName(''); checkPackageName('   '); return window.__snapshot(); }",
                       ).flatMap(json => ZIO.fromEither(json.fromJson[UiState]).mapError(RuntimeException(_)))
      nonEmpty      <- evalString(
                         page,
                         "() => { window.__existsCalls=0; checkPackageName('jquery'); return window.__snapshot(); }",
                       ).flatMap(json => ZIO.fromEither(json.fromJson[UiState]).mapError(RuntimeException(_)))
      _             <- ZIO.sleep(300.millis)
      versionsLoaded <- state(page)
      _             <- page.evaluate(
                         "() => { var s=document.getElementById('newWebJarVersion'); s.value='1.0.0'; s.dispatchEvent(new Event('change',{bubbles:true})); return 'configured'; }",
                         isFunction = true,
                       )
      _             <- page.click("#deployButton")
      deployFailure <- evalString(
                         page,
                         """() => {
                           |  window.__eventSource.emit('message','The package license could not be determined.');
                           |  window.__eventSource.emit('message','[deploy-failure:user-input]');
                           |  window.__eventSource.emit('message','Tracking issue: https://example.test/issues/42');
                           |  return window.__snapshot();
                           |}""".stripMargin,
                       ).flatMap(json => ZIO.fromEither(json.fromJson[UiState]).mapError(RuntimeException(_)))
    yield assertTrue(
      !initial.npm && !initial.classic && initial.disabled && !initial.jqueryPresent,
      searchLoaded.searchCalls == 1 && searchLoaded.listTitle == "Search Results" && searchLoaded.listText.contains("React"),
      popularLoaded.popularCalls == 1 && popularLoaded.listTitle == "Popular WebJars" && popularLoaded.listText.contains("Bootstrap"),
      mavenSelected.buildInstructions.contains("<groupId>org.webjars.npm</groupId>"),
      mavenSelected.buildInstructions.contains("<artifactId>bootstrap</artifactId>"),
      npmLabel.npm && !npmLabel.classic && !npmLabel.disabled,
      !classicLaunch.npm && classicLaunch.classic && !classicLaunch.disabled && classicLaunch.name == "jquery",
      npmLaunch.npm && !npmLaunch.classic && !npmLaunch.disabled && npmLaunch.name == "@scope/package",
      emptyNames.calls == 0 && emptyNames.versionDisabled,
      nonEmpty.calls == 1,
      !versionsLoaded.versionDisabled,
      versionsLoaded.versionValues == List("", "2.0.0", "1.0.0"),
      !deployFailure.alertHidden,
      deployFailure.alertTitle == "Check the deployment details",
      deployFailure.alertMessage == "The package license could not be determined.",
      deployFailure.alertWarning,
      !deployFailure.deployLog.contains("[deploy-failure:"),
      deployFailure.trackingHref == "https://example.test/issues/42",
      deployFailure.errors.isEmpty,
    )

  override def spec =
    if !ChekhovSupport.available then
      suite("NewWebJarBrowserSpec")(
        test("skipped — needs a system chromium/chrome plus Node and npm")(assertCompletes)
      )
    else
      suite("NewWebJarBrowserSpec")(
        test("modal JavaScript preserves deterministic accessible state and friendly deploy failures")(program)
      ).provide(
        Server.defaultWith(_.onAnyOpenPort),
        ChekhovSupport.configLayer >>> PlaywrightDriver.suiteLayers,
      ) @@ withLiveClock @@ timeout(180.seconds) @@ sequential
