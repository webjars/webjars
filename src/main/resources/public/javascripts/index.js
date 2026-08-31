function all(selector, root = document) {
  return Array.from(root.querySelectorAll(selector));
}

function byId(id) {
  return document.getElementById(id);
}

function setupWebJarList() {
  all(".file-list-link").forEach(function (link) {
    link.onclick = onFileList;
  });

  all("#buildtoolselect input").forEach(function (input) {
    input.onclick = function () {
      updateAllDetails(input.value);
    };
  });

  const selectedBuildTool = document.querySelector("input[type=radio][name=buildtool]:checked");
  if (selectedBuildTool) {
    updateAllDetails(selectedBuildTool.value);
  }
}

let currentListController = null;

async function loadList(url, titleText) {
  if (currentListController) {
    currentListController.abort();
  }

  const controller = new AbortController();
  currentListController = controller;

  try {
    const response = await fetch(url, {
      headers: { Accept: "text/html" },
      signal: controller.signal,
    });
    if (!response.ok) {
      throw new Error(`List request failed with status ${response.status}`);
    }

    const data = await response.text();
    if (currentListController !== controller) {
      return;
    }

    byId("listTitle").textContent = titleText;
    byId("webJarList").innerHTML = data;
    setupWebJarList();
  } catch (error) {
    if (error.name !== "AbortError") {
      console.error("Could not load the WebJar list", error);
    }
  }
}

function searchWebJars(query, groupIds) {
  const params = new URLSearchParams({ query: query });
  groupIds.forEach(function (groupId) {
    params.append("groupId", groupId);
  });
  loadList(`/search?${params.toString()}`, "Search Results");
}

function loadPopular() {
  loadList("/popular", "Popular WebJars");
}

let currentFileListController = null;

async function onFileList(event) {
  if (event.button !== 0 || event.metaKey || event.ctrlKey) {
    return;
  }

  event.preventDefault();
  if (currentFileListController) {
    currentFileListController.abort();
  }
  const controller = new AbortController();
  currentFileListController = controller;

  const link = event.currentTarget;
  const row = link.closest("tr");
  const modalElement = byId("fileListModal");
  const modalBody = modalElement.querySelector(".modal-body");

  byId("fileListModalLabel").textContent = `Files for ${row.dataset.artifact}`;
  modalBody.textContent = "Loading...";
  bootstrap.Modal.getOrCreateInstance(modalElement).show();

  try {
    const response = await fetch(link.href, {
      headers: { Accept: "text/html" },
      signal: controller.signal,
    });
    if (!response.ok) {
      throw new Error(`File list request failed with status ${response.status}`);
    }
    const html = await response.text();
    if (currentFileListController === controller) {
      modalBody.innerHTML = html;
    }
  } catch (error) {
    if (error.name !== "AbortError" && currentFileListController === controller) {
      modalBody.textContent = "The file list is unavailable at this time.";
      console.error("Could not load the WebJar file list", error);
    }
  }
}

function changeVersion(event) {
  const selectedBuildTool = document.querySelector("input[type=radio][name=buildtool]:checked");
  const row = event.target.closest("tr");
  if (selectedBuildTool && row) {
    updateDetails(selectedBuildTool.value, row);
  }
}

function updateDetails(buildTool, row) {
  const groupId = row.dataset.group;
  const artifactId = row.dataset.artifact;
  const webJarVersion = row.querySelector(".versions");
  if (!webJarVersion) {
    return;
  }

  const version = webJarVersion.value;
  let instructions = "";
  switch (buildTool) {
    case "buildr":
      instructions = `'${groupId}:${artifactId}:jar:${version}'`;
      break;
    case "gradle":
      instructions = `runtimeOnly("${groupId}:${artifactId}:${version}")`;
      break;
    case "grape":
      instructions = `@Grapes(\n    @Grab(group='${groupId}', module='${artifactId}', version='${version}')\n)`;
      break;
    case "ivy":
      instructions = `<dependency org="${groupId}" name="${artifactId}" rev="${version}" />`;
      break;
    case "leiningen":
      instructions = `${groupId}/${artifactId} "${version}"`;
      break;
    case "maven":
      instructions = `<dependency>\n    <groupId>${groupId}</groupId>\n    <artifactId>${artifactId}</artifactId>\n    <version>${version}</version>\n</dependency>`;
      break;
    case "sbt":
      instructions = `"${groupId}" % "${artifactId}" % "${version}"`;
      break;
  }

  const instructionsElement = row.querySelector(".build-instructions pre");
  if (instructionsElement) {
    instructionsElement.textContent = instructions;
  }

  const selectedOption = webJarVersion.selectedOptions[0];
  const numFiles = selectedOption && selectedOption.dataset.numfiles
    ? selectedOption.dataset.numfiles
    : "List";
  const filesLink = document.createElement("a");
  filesLink.href = `/listfiles/${groupId}/${artifactId}/${encodeURIComponent(version)}`;
  filesLink.className = "file-list-link";
  filesLink.textContent = `${numFiles} Files`;
  filesLink.onclick = onFileList;

  const filesElement = row.querySelector(".files");
  if (filesElement) {
    filesElement.replaceChildren(filesLink);
  }
}

function updateAllDetails(buildTool) {
  all("tr[data-artifact]").forEach(function (row) {
    updateDetails(buildTool, row);
  });
}

function webJarType() {
  const selected = document.querySelector("input[type=radio][name=new_webjar_catalog]:checked");
  return selected ? selected.value : undefined;
}

function parsePackageOrRepoName(value) {
  const packageOrUrl = (value || "").trim().split("#");
  const data = { packageOrRepo: packageOrUrl[0].trim() };
  if (packageOrUrl.length === 2) {
    data.branch = packageOrUrl[1].trim();
  }
  return data;
}

function getPackageOrRepoName() {
  return parsePackageOrRepoName(byId("newWebJarName").value);
}

function setDeployUiVisible(visible) {
  byId("deployLogSection").classList.toggle("d-none", !visible);
  byId("deployButton").classList.toggle("d-none", !visible);
}

function markNameInvalid(message) {
  const nameInput = byId("newWebJarName");
  nameInput.classList.remove("is-valid");
  nameInput.classList.add("is-invalid");
  byId("newWebJarNameError").textContent = message || "";
}

function markNameValid() {
  const nameInput = byId("newWebJarName");
  nameInput.classList.remove("is-invalid");
  nameInput.classList.add("is-valid");
  byId("newWebJarNameError").textContent = "";
}

function clearDeployError() {
  byId("deployError").classList.add("d-none");
  byId("deployErrorMessage").textContent = "";
  byId("deployErrorTracking").classList.add("d-none");
  const trackingUrl = byId("deployErrorTrackingUrl");
  trackingUrl.removeAttribute("href");
  trackingUrl.textContent = "";
}

function showDeployError(category, message) {
  let title;
  let alertClass;

  if (category === "user-input") {
    title = "Check the deployment details";
    alertClass = "alert-warning";
  } else if (category === "transient") {
    title = "Deployment temporarily unavailable";
    alertClass = "alert-warning";
  } else {
    title = "Deployment failed";
    alertClass = "alert-danger";
  }

  byId("deployErrorTitle").textContent = title;
  byId("deployErrorMessage").textContent = message || "The WebJar could not be deployed.";
  const deployError = byId("deployError");
  deployError.classList.remove("alert-danger", "alert-warning", "d-none");
  deployError.classList.add(alertClass);
}

function showDeployTrackingIssue(url) {
  const trackingUrl = byId("deployErrorTrackingUrl");
  trackingUrl.href = url;
  trackingUrl.textContent = url;
  byId("deployErrorTracking").classList.remove("d-none");
}

function notDeployableMessage(packageName) {
  if (webJarType() === "classic") {
    return `The Classic WebJar ${packageName} Can't Be Deployed This Way`;
  }
  return `The NPM Package ${packageName} was not found`;
}

function resetVersionSelect(label = "Select a version") {
  const versionSelect = byId("newWebJarVersion");
  versionSelect.replaceChildren(new Option(label, "", true, true));
  versionSelect.disabled = true;
  byId("deployButton").disabled = true;
}

function resetClassicGuidance() {
  byId("classicVersions").classList.add("d-none");
  byId("classicVersionsList").replaceChildren();
  byId("classicNewVersionLink").classList.add("d-none");
  byId("classicNewWebJarLink").classList.add("d-none");
}

let nameCheckSequence = 0;
let currentNameController = null;
let currentVersionsController = null;

function abortPackageRequests() {
  if (currentNameController) {
    currentNameController.abort();
    currentNameController = null;
  }
  if (currentVersionsController) {
    currentVersionsController.abort();
    currentVersionsController = null;
  }
}

async function loadPackageVersions(sequence, packageOrRepoName) {
  if (currentVersionsController) {
    currentVersionsController.abort();
  }

  const controller = new AbortController();
  currentVersionsController = controller;
  const versionSelect = byId("newWebJarVersion");
  resetVersionSelect("Loading versions...");
  versionSelect.setAttribute("aria-busy", "true");

  const params = new URLSearchParams({
    webJarType: webJarType() || "",
    name: packageOrRepoName.packageOrRepo,
  });
  if (packageOrRepoName.branch !== undefined) {
    params.set("branch", packageOrRepoName.branch);
  }

  try {
    const response = await fetch(`/versions?${params.toString()}`, {
      headers: { Accept: "application/json" },
      signal: controller.signal,
    });
    if (!response.ok) {
      throw new Error(`Versions request failed with status ${response.status}`);
    }

    const versions = await response.json();
    if (sequence !== nameCheckSequence) {
      return;
    }
    if (!Array.isArray(versions)) {
      throw new Error("Versions response was not an array");
    }

    resetVersionSelect(versions.length > 0 ? "Select a version" : "No versions available");
    versions.forEach(function (version) {
      versionSelect.add(new Option(version, version));
    });
    versionSelect.disabled = versions.length === 0;
  } catch (error) {
    if (error.name !== "AbortError" && sequence === nameCheckSequence) {
      resetVersionSelect("Versions unavailable");
      markNameInvalid("Versions are unavailable at this time");
      console.error("Could not load package versions", error);
    }
  } finally {
    if (sequence === nameCheckSequence) {
      versionSelect.removeAttribute("aria-busy");
    }
  }
}

async function checkPackageName(packageName) {
  packageName = (packageName || "").trim();

  if (packageName.length === 0) {
    nameCheckSequence += 1;
    abortPackageRequests();
    const nameInput = byId("newWebJarName");
    nameInput.classList.remove("is-valid", "is-invalid");
    byId("newWebJarNameError").textContent = "";
    byId("newWebJarNameSpinner").classList.remove("spinner-border");
    resetVersionSelect();
    resetClassicGuidance();
    setDeployUiVisible(true);
    return;
  }

  abortPackageRequests();
  const sequence = ++nameCheckSequence;
  const controller = new AbortController();
  currentNameController = controller;

  const nameInput = byId("newWebJarName");
  nameInput.classList.remove("is-valid", "is-invalid");
  byId("newWebJarNameError").textContent = "";
  byId("newWebJarNameSpinner").classList.add("spinner-border");
  resetVersionSelect();
  resetClassicGuidance();
  setDeployUiVisible(true);

  const selectedType = webJarType() || "";
  const packageOrRepoName = parsePackageOrRepoName(packageName);
  const params = new URLSearchParams({ webJarType: selectedType, name: packageOrRepoName.packageOrRepo });

  try {
    const response = await fetch(`/exists?${params.toString()}`, {
      headers: { Accept: "application/json" },
      signal: controller.signal,
    });
    if (!response.ok) {
      throw new Error(`Package check failed with status ${response.status}`);
    }

    const data = await response.json();
    if (sequence !== nameCheckSequence) {
      return;
    }

    byId("newWebJarNameSpinner").classList.remove("spinner-border");
    if (data.deployable) {
      markNameValid();
      await loadPackageVersions(sequence, packageOrRepoName);
      return;
    }

    markNameInvalid(data.error || notDeployableMessage(packageName));
    const versions = Array.isArray(data.versions) ? data.versions : [];
    if (versions.length > 0) {
      const list = byId("classicVersionsList");
      versions.forEach(function (version) {
        const item = document.createElement("li");
        item.textContent = version;
        list.append(item);
      });
      byId("classicVersions").classList.remove("d-none");
    }

    if (selectedType === "classic") {
      if (versions.length > 0) {
        byId("classicNewVersionUrl").href = `https://github.com/webjars/${packageName}/issues/new`;
        byId("classicNewVersionLink").classList.remove("d-none");
      } else {
        byId("classicNewWebJarLink").classList.remove("d-none");
      }
    }
    setDeployUiVisible(false);
    resetVersionSelect();
  } catch (error) {
    if (error.name !== "AbortError" && sequence === nameCheckSequence) {
      markNameInvalid("Deployment is unavailable at this time");
      byId("newWebJarNameSpinner").classList.remove("spinner-border");
      resetVersionSelect();
    }
  }
}

function handleSearch() {
  const searchText = byId("search").value.trim();
  const groupIds = all("input[name='search_catalog[]']:checked").map(function (input) {
    return input.value;
  });

  if (searchText === "") {
    byId("clearSearch").style.display = "none";
    loadPopular();
  } else {
    byId("clearSearch").style.display = "";
    searchWebJars(searchText, groupIds);
  }
}

function clearSearch() {
  byId("search").value = "";
  byId("clearSearch").style.display = "none";
  loadPopular();
}

function initializeIndexPage() {
  setupWebJarList();

  const searchInput = byId("search");
  if (searchInput) {
    searchInput.addEventListener("input", function () {
      const length = searchInput.value.length;
      if (length === 0 || length > 2) {
        handleSearch();
      } else {
        byId("clearSearch").style.display = "";
      }
    });
    searchInput.addEventListener("keydown", function (event) {
      if (event.key === "Enter") {
        event.preventDefault();
        handleSearch();
      }
    });
  }

  all("input[name='search_catalog[]']").forEach(function (input) {
    input.addEventListener("change", handleSearch);
  });

  const clearSearchButton = byId("clearSearch");
  if (clearSearchButton) {
    clearSearchButton.addEventListener("click", clearSearch);
    clearSearchButton.addEventListener("keydown", function (event) {
      if (event.key === "Enter" || event.key === " ") {
        event.preventDefault();
        clearSearch();
      }
    });
  }

  let nameDebounceTimer = null;
  all("input[type=radio][name=new_webjar_catalog]").forEach(function (input) {
    input.addEventListener("change", function () {
      clearTimeout(nameDebounceTimer);
      const nameInput = byId("newWebJarName");
      nameInput.disabled = false;
      if (nameInput.value.length > 0) {
        checkPackageName(nameInput.value);
      }
    });
  });

  byId("newWebJarName").addEventListener("input", function (event) {
    clearTimeout(nameDebounceTimer);
    if (event.target.value.trim().length === 0) {
      checkPackageName(event.target.value);
    } else {
      nameDebounceTimer = setTimeout(function () {
        checkPackageName(event.target.value);
      }, 600);
    }
  });

  byId("newWebJarVersion").addEventListener("change", function (event) {
    byId("deployButton").disabled = event.target.value.length === 0;
  });

  byId("deployButton").addEventListener("click", function (event) {
    event.preventDefault();

    const deployLog = byId("deployLog");
    let latestMessage = "";
    clearDeployError();

    function log(message) {
      const failureMatch = message.trim().match(/^\[deploy-failure:(user-input|transient|systemic)\]$/);
      if (failureMatch !== null) {
        showDeployError(failureMatch[1], latestMessage);
        return;
      }

      const trackingPrefix = "Tracking issue: ";
      const trackingIdx = message.indexOf(trackingPrefix);
      if (trackingIdx === 0) {
        const rest = message.slice(trackingPrefix.length).trim();
        const urlEnd = rest.search(/\s/);
        const url = urlEnd === -1 ? rest : rest.slice(0, urlEnd);
        const trailing = urlEnd === -1 ? "\n" : rest.slice(urlEnd);
        showDeployTrackingIssue(url);
        deployLog.append(document.createTextNode(trackingPrefix));
        const link = document.createElement("a");
        link.href = url;
        link.target = "_blank";
        link.rel = "noopener noreferrer";
        link.textContent = url;
        deployLog.append(link, document.createTextNode(trailing));
      } else {
        latestMessage = message.trim();
        deployLog.append(document.createTextNode(message));
      }
      deployLog.scrollTop = deployLog.scrollHeight;
    }

    byId("deployButton").disabled = true;
    const packageOrRepoName = getPackageOrRepoName();
    const artifactId = packageOrRepoName.packageOrRepo;
    const version = byId("newWebJarVersion").value;

    deployLog.textContent = "Starting Deploy\n";
    const deployUrl = `/deploy?webJarType=${webJarType()}&nameOrUrlish=${encodeURIComponent(artifactId)}&version=${encodeURIComponent(version)}`;
    const source = new EventSource(deployUrl);

    source.addEventListener("message", function (messageEvent) {
      if (messageEvent.data.length > 0) {
        const message = messageEvent.data.endsWith("\n") ? messageEvent.data : `${messageEvent.data}\n`;
        log(message);
      }
    });
    source.addEventListener("error", function () {
      source.close();
      byId("deployButton").disabled = false;
    });
  });

  byId("newWebJarModal").addEventListener("show.bs.modal", function (event) {
    clearTimeout(nameDebounceTimer);
    nameCheckSequence += 1;
    abortPackageRequests();
    byId("deployButton").disabled = true;
    setDeployUiVisible(true);
    clearDeployError();
    resetClassicGuidance();

    const trigger = event.relatedTarget
      ? event.relatedTarget.closest("[data-bs-target='#newWebJarModal']")
      : null;
    const selectedWebJarType = trigger ? trigger.dataset.webjarType : undefined;
    const artifactId = trigger ? trigger.dataset.artifactId : undefined;
    const name = trigger ? trigger.dataset.name : undefined;

    all("input[type=radio][name=new_webjar_catalog]").forEach(function (input) {
      input.checked = false;
    });

    const nameInput = byId("newWebJarName");
    nameInput.value = "";
    nameInput.disabled = true;
    nameInput.classList.remove("is-valid", "is-invalid");
    byId("newWebJarNameError").textContent = "";
    resetVersionSelect();
    byId("deployLog").textContent = "";

    let inputValue;
    if (selectedWebJarType === "classic") {
      const classicInput = document.querySelector("input[name=new_webjar_catalog][value='classic']");
      classicInput.checked = true;
      classicInput.dispatchEvent(new Event("change", { bubbles: true }));
      inputValue = artifactId;
    } else if (selectedWebJarType === "npm") {
      const npmInput = document.querySelector("input[name=new_webjar_catalog][value='npm']");
      npmInput.checked = true;
      npmInput.dispatchEvent(new Event("change", { bubbles: true }));
      inputValue = name;
    }

    if (inputValue !== undefined) {
      nameInput.value = inputValue;
      checkPackageName(inputValue);
    }
  });
}

if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", initializeIndexPage);
} else {
  initializeIndexPage();
}

function cometMessage(event) {
  console.log(`Received event: ${event}`);
}
