var webjars = [];
var buildTool = "sbt" ;

$(function() {

  // list files modal
  $(".file-list-link").click(onFileList);

  // build tool change handler
  $("#buildtoolselect").find("label").click(function(e) {
    // update for each webjar
    buildTool = $(this).attr("data-value");
    updateAllDetails();
  });

  $("#search").keyup(function() {
    var searchText = $(this).val();

    if (searchText == "") {
      $("#clearSearch").hide();
    }
    else {
      $("#clearSearch").show();
    }

    if (searchText.length == 1) {
      return;
    }
    filterWebJars(searchText);
  });

  $("#clearSearch").click(function(){
    $("#search").val("");
    $(this).hide();
    filterWebJars("");
  });

});

function filterWebJars(search) {
  $("tr[data-artifact]").each(function() {
    var row = $(this);
    var shouldHide = row.data("artifact").toLowerCase().indexOf(search.toLowerCase()) === -1;
    row.toggleClass('hide', shouldHide);
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

/*
function webJarRow(groupId, artifactId) {
//  console.log($("tr[data-group='" + groupId + "'][data-artifact='" + artifactId + "']"));
  return $("tr[data-group='" + groupId + "'][data-artifact='" + artifactId + "']");
}
*/

function changeVersion(event) {
  updateDetails($(event.target).parents("tr"));
}

function updateDetails(row) {
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

function updateAllDetails() {
  $("tr[data-artifact]").each(function() {
    updateDetails($(this));
  });
}
