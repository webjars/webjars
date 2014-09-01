$(function() {
  $(".file-list-link").click(onFileList);

  $("#buildtoolselect").find("label").click(function(e) {
    // update for each webjar
    buildTool = $(this).attr("data-value");
    $.each(webjars, function(i, webjar) { updateDetails(webjar); });
  });

  $("tr").each(function (i, tr) {
    var webjar = getWebjar($(tr).data("artifact"));
    if (webjar != undefined) {
      webjar['row'] = $(tr);
    }
  });
});

function onFileList(event) {
  // allow middle clicks to open in a new tab
  if (event.button == 0) {
    event.preventDefault();
    $("#fileListModalLabel").text("Files for " + $(this).parents("tr").data("artifact"));
    $("#fileListModal .modal-body").text("Loading...");
    $("#fileListModal").removeData("modal");
    $("#fileListModal .modal-body").load($(this).attr("href"));
    $('#fileListModal').modal('show');
  }
}

function selectedVersion(artifactId) {
  return row(artifactId).find(".versions").val();
}

function row(artifactId) {
  return getWebjar(artifactId)['row'];
}

function changeVersion(event) {
  var artifact = $(event.target).parents("tr").data("artifact");
  updateDetails(getWebjar(artifact));
}

function getWebjar(artifact) {
  return $.grep(webjars, function(webjar, i) { return webjar.artifactId == artifact })[0];
}

function updateDetails(webjar) {
  var webjarVersion = selectedVersion(webjar.artifactId);

  // update instructions
  var instructions = "";
  switch (buildTool) {
    case "buildr":
      instructions = "'org.webjars:" + webjar.artifactId + ":jar:" + webjarVersion + "'";
      break;
    case "gradle":
      instructions = "compile 'org.webjars:" + webjar.artifactId + ":" + webjarVersion + "'";
      break;
    case "grape":
      instructions = "@Grapes(\n" +
        "    @Grab(group='org.webjars', module='" + webjar.artifactId + "', version='" + webjarVersion + "')\n" +
        ")";
      break;
    case "ivy":
      instructions = '<dependency org="org.webjars" name="' + webjar.artifactId + '" rev="' + webjarVersion + '" />';
      break;
    case "leiningen":
      instructions = "org.webjars/" + webjar.artifactId + ' "' + webjarVersion + '"';
      break;
    case "maven":
      instructions = "<dependency>\n" +
        "    <groupId>org.webjars</groupId>\n" +
        "    <artifactId>" + webjar.artifactId + "</artifactId>\n" +
        "    <version>" + webjarVersion + "</version>\n" +
        "</dependency>";
      break;
    case "sbt":
      instructions = '"org.webjars" % "' + webjar.artifactId + '" % "' + webjarVersion + '"';
      break;
  }

  row(webjar.artifactId).find(".build-instructions pre").text(instructions);

  // update files link
  var filesLink = $("<a>").attr("href", "/listfiles/" + webjar.artifactId + "/" + webjarVersion).addClass("file-list-link");
  filesLink.click(onFileList);
  var version = $.grep(webjar.versions, function(version, i) { return version.number == webjarVersion; })[0];
  if (version.numFiles != undefined) {
    filesLink.text(version.numFiles + " Files");
  }
  else {
    filesLink.text("List Files");
  }

  row(webjar.artifactId).find(".files").empty().append(filesLink);
}