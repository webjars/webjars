$(function() {

  setupWebJarList();

  $("#search").keyup(function() {
    var searchText = $(this).val();

    if (searchText == "") {
      $("#clearSearch").hide();
    }
    else {
      $("#clearSearch").show();
    }

    if (searchText.length > 2) {
      searchWebJars(searchText);
    }
  });

  $("#clearSearch").click(function(){
    $("#search").val("");
    $(this).hide();

    $.get("/popular", function(data) {
      $("#listTitle").html("Popular WebJars");

      $("#webJarList").html(data);

      setupWebJarList();
    });
  });

});

function setupWebJarList() {
  // list files modal
  $(".file-list-link").click(onFileList);

  // build tool change handler
  $("#buildtoolselect").find("label").click(function(e) {
    // update for each webjar
    buildTool = $(this).attr("data-value");
    updateAllDetails(buildTool);
  });
}

function searchWebJars(query) {
  $.get("/search?query=" + query, function(data) {
    $("#listTitle").html("Search Results");

    $("#webJarList").html(data);

    setupWebJarList();
  });
}

function onFileList(event) {
  // allow middle clicks to open in a new tab
  if ((event.button == 0) && (!event.metaKey)) {
    event.preventDefault();
    $("#fileListModalLabel").text("Files for " + $(this).parents("tr").data("artifact"));
    $("#fileListModal .modal-body").text("Loading...");
    $("#fileListModal").removeData("modal");
    $("#fileListModal .modal-body").load($(this).attr("href"));
    $("#fileListModal").modal("show");
  }
}

function changeVersion(event) {
  updateDetails($(event.target).parents("tr"));
}

function updateDetails(buildTool, row) {
  var groupId = row.data("group");
  var artifactId = row.data("artifact");

  var webJarVersion = row.find(".versions");

  // update instructions
  var instructions = "";
  switch (buildTool) {
    case "buildr":
      instructions = "'" + groupId + ":" + artifactId + ":jar:" + webJarVersion.val() + "'";
      break;
    case "gradle":
      instructions = "compile '" + groupId + ":" + artifactId + ":" + webJarVersion.val() + "'";
      break;
    case "grape":
      instructions = "@Grapes(\n" +
        "    @Grab(group='" + groupId + "', module='" + artifactId + "', version='" + webJarVersion.val() + "')\n" +
        ")";
      break;
    case "ivy":
      instructions = '<dependency org="' + groupId + '" name="' + artifactId + '" rev="' + webJarVersion.val() + '" />';
      break;
    case "leiningen":
      instructions = groupId + "/" + artifactId + ' "' + webJarVersion.val() + '"';
      break;
    case "maven":
      instructions = "<dependency>\n" +
        "    <groupId>" + groupId + "</groupId>\n" +
        "    <artifactId>" + artifactId + "</artifactId>\n" +
        "    <version>" + webJarVersion.val() + "</version>\n" +
        "</dependency>";
      break;
    case "sbt":
      instructions = '"' + groupId + '" % "' + artifactId + '" % "' + webJarVersion.val() + '"';
      break;
  }

  row.find(".build-instructions pre").text(instructions);

  // update files link
  var filesLink = $("<a>").attr("href", "/listfiles/" + groupId + "/" + artifactId + "/" + webJarVersion.val()).addClass("file-list-link");
  filesLink.click(onFileList);
  filesLink.text(webJarVersion.find(":selected").data("numfiles") + " Files");
  row.find(".files").empty().append(filesLink);
}

function updateAllDetails(buildTool) {
  $("tr[data-artifact]").each(function() {
    updateDetails(buildTool, $(this));
  });
}
