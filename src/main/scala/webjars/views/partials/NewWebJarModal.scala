package webjars.views.partials

import zio.http.template2.*

object NewWebJarModal:
  def apply(): Dom = Dom.raw(
    """<div id="newWebJarModal" class="modal fade" tabindex="-1" aria-labelledby="newWebJarModalLabel" aria-hidden="true" data-bs-backdrop="static">
    <div class="modal-dialog modal-lg">
        <div class="modal-content">
            <div class="modal-header">
                <h1 id="newWebJarModalLabel" class="modal-title fs-5">Add a New WebJar</h1>
                <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
            </div>

            <div class="modal-body">
                <form>
                    <fieldset class="d-inline-flex flex-row align-items-center gap-3 mb-3">
                        <legend class="col-form-label">WebJar Type:</legend>
                        <div class="form-check mb-0">
                            <input id="npm" class="form-check-input" type="radio" name="new_webjar_catalog" value="npm" checked>
                            <label class="form-check-label" for="npm">NPM</label>
                        </div>
                        <div class="form-check mb-0">
                            <input id="classic" class="form-check-input" type="radio" name="new_webjar_catalog" value="classic">
                            <label class="form-check-label" for="classic">Classic</label>
                        </div>
                    </fieldset>
                </form>

                <div class="webjar-deploy">
                    <form id="webjar-deploy" class="row g-3 mb-3" onsubmit="return false;">
                        <div class="col-md-6 position-relative">
                            <input id="newWebJarName" type="text" class="form-control form-control-sm" aria-describedby="newWebJarNameStatus newWebJarNameError" placeholder="Package Name or Git Repo URL">
                            <span id="newWebJarNameSpinner" class="spinner-border-sm" aria-hidden="true"></span>
                            <div id="newWebJarNameError" class="invalid-feedback"></div>
                        </div>
                        <div class="col-md-6">
                            <label for="newWebJarVersion" class="control-label visually-hidden">Package Version</label>
                            <select id="newWebJarVersion" class="form-select form-select-sm" disabled data-placeholder="Version"></select>
                        </div>
                    </form>

                    <div id="classicVersions" class="d-none mb-3">
                        <h2 class="fs-6 mb-2">Available Versions on Maven Central</h2>
                        <ul id="classicVersionsList" class="list-unstyled small mb-0"></ul>
                    </div>

                    <p id="classicNewVersionLink" class="d-none small mb-3">
                        If you need a new version of an existing Classic WebJar, <a id="classicNewVersionUrl" target="_blank" rel="noopener noreferrer">create a request on the WebJar's repo</a>.
                    </p>

                    <p id="classicNewWebJarLink" class="d-none small mb-3">
                        If you need a new Classic WebJar, <a href="https://github.com/webjars/webjars-classic/issues/new" target="_blank" rel="noopener noreferrer">create a request to have it created</a>.
                    </p>

                    <div id="deployLogSection">
                        <h2 class="fs-6 mb-2">Deploy Log</h2>
                        <pre id="deployLog" class="border" disabled></pre>
                    </div>
                </div>
            </div>

            <div class="modal-footer webjar-deploy">
                <button id="deployButton" type="button" class="btn btn-primary" disabled>Deploy!</button>
            </div>
        </div>
    </div>
</div>"""
  )
