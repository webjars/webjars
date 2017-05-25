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

function searchWebJars(query, groupIds) {
  var groupIdsQueryString = groupIds.reduce(function(acc, val) {
    acc += "&groupId=" + val;
    return acc;
  }, "");
  $.get("/search?query=" + query + groupIdsQueryString, function(data) {
    $("#listTitle").html("Search Results");

    $("#webJarList").html(data);

    setupWebJarList();
  });
}

function onFileList(event) {
  // allow middle clicks to open in a new tab
  if ((event.button === 0) && (!event.metaKey)) {
    event.preventDefault();
    $("#fileListModalLabel").text("Files for " + $(this).parents("tr").data("artifact"));
    $("#fileListModal .modal-body").text("Loading...");
    $("#fileListModal").removeData("modal");
    $("#fileListModal .modal-body").load($(this).attr("href"));
    $("#fileListModal").modal("show");
  }
}

function changeVersion(event) {
  var buildtool = $("input[type=radio][name=buildtool]").val();
  var row = $(event.target).parents("tr");
  updateDetails(buildtool, row);
}

function updateDetails(buildTool, row) {
  var groupId = row.attr("data-group");
  var artifactId = row.attr("data-artifact");

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

// from: http://stackoverflow.com/a/105074/77409
function guid() {
  function s4() {
    return Math.floor((1 + Math.random()) * 0x10000)
      .toString(16)
      .substring(1);
  }
  return s4() + s4() + "-" + s4() + "-" + s4() + "-" +
    s4() + "-" + s4() + s4() + s4();
}

function webJarType() {
  return $("input[type=radio][name=new_webjar_catalog]:checked").val();
}

function getPackageOrRepoName() {
  var packageOrUrl = $("#newWebJarName").val().split("#");

  var data = {};

  if (packageOrUrl.length > 0) {
    data.packageOrRepo = packageOrUrl[0];
  }

  if (packageOrUrl.length === 2) {
    data.branch = packageOrUrl[1];
  }

  return data;
}

function checkPackageName(packageName) {
  $("#newWebJarName").parent().removeClass("has-error has-success");
  $("#newWebJarNameFeedback").removeClass("glyphicon-ok glyphicon-remove hidden").addClass("glyphicon-refresh spin");
  $("#newWebJarVersion").select2("val", "");
  $("#newWebJarVersion").select2("enable", false);

  $.ajax({
    url: "/_" + webJarType() + "/exists?name=" + packageName,
    success: function(data, status) {
      $("#newWebJarName").parent().addClass("has-success");
      $("#newWebJarNameFeedback").addClass("glyphicon-ok").removeClass("glyphicon-refresh spin");
      $("#newWebJarVersion").select2("enable", true);
    },
    error: function(data, status) {
      $("#newWebJarName").parent().addClass("has-error");
      $("#newWebJarNameFeedback").addClass("glyphicon-remove").removeClass("glyphicon-refresh spin");
      $("#newWebJarVersion").select2("enable", false);
    }
  });
}

function handleSearch() {
  var searchText = $("#search").val();
  var groupIds = $("input[name='search_catalog[]']:checked").map(function() {
    return $(this).val();
  }).get();

  if (searchText === "") {
    $("#clearSearch").hide();
  }
  else {
    $("#clearSearch").show();
    searchWebJars(searchText, groupIds);
  }
}

$(function() {

  setupWebJarList();

  $("#search").keyup(function(event) {
    if ((event.keyCode === 13) || (this.value.length > 2)) {
      handleSearch();
    }
  });

  $("input[name='search_catalog[]").change(function() {
    handleSearch();
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

  $("input[type=radio][name=new_webjar_catalog]").change(function() {
    if (this.value === "org.webjars") {
      $(".npm-bower-deploy").hide();
      $(".classic-deploy").show();
    }
    else {
      $(".npm-bower-deploy").show();
      $(".classic-deploy").hide();
    }
  });

  $("#newWebJarName").typeWatch({
    callback: checkPackageName,
    wait: 600,
    captureLength: 0
  });

  $("#newWebJarVersion").select2({
    placeholder: "Version",
    id: function(item) {
      return item;
    },
    query: function(query) {
      var packageOrRepoName = getPackageOrRepoName();

      var url = "/_" + webJarType() + "/versions?name=" + packageOrRepoName.packageOrRepo;

      if (packageOrRepoName.branch !== undefined) {
        url += "&branch=" + packageOrRepoName.branch;
      }

      $.getJSON(url, function(data) {
        query.callback({results: data});
      });
    },
    formatResult: function(item) {
      return item;
    },
    formatSelection: function(item) {
      return item;
    }
  }).on("change", function(event) {
    $("#deployButton").attr("disabled", false);
  });

  $("#deployButton").click(function(event) {
    event.preventDefault();

    var deployLog = $("#deployLog");
    function log(message) {
      var t = deployLog.text();
      deployLog.text(message + "\n" + t);
    }

    $("#deployButton").attr("disabled", true);

    var packageOrRepoName = getPackageOrRepoName();

    var artifactId = packageOrRepoName.packageOrRepo;
    var version = $("#newWebJarVersion").select2("val");

    var pusher = new Pusher(window.pusherKey);
    var channelId = guid();
    var channel = pusher.subscribe(channelId);
    channel.bind("update", function(data) {
      log(data);
    });
    channel.bind("success", function(data) {
      log(data);
      pusher.disconnect();
      $("#deployButton").attr("disabled", false);
    });
    channel.bind("failure", function(data) {
      log(data);
      pusher.disconnect();
      $("#deployButton").attr("disabled", false);
    });

    deployLog.text("Starting Deploy");

    var deployUrl = "/_" + webJarType() + "/deploy?name=" + encodeURIComponent(artifactId) + "&version=" + encodeURIComponent(version) + "&channelId=" + channelId;

    $.ajax(deployUrl, {
      method: "post",
      success: function(data) {
        console.log(data);
      }
    });
  });

  $("#newWebJarModal").on("show.bs.modal", function (event) {
    var artifactId = $(event.relatedTarget).data("artifact-id");
    var groupId = $(event.relatedTarget).data("group-id");
    if (groupId !== undefined) {
      $("input[type=radio][name=new_webjar_catalog]").prop("checked", false);
      $("input[type=radio][name=new_webjar_catalog][value='" + groupId + "']").prop("checked", true).trigger("change");
    }

    if (artifactId !== undefined) {
      $("#newWebJarName").val(artifactId);
      checkPackageName(artifactId);
    }
    else {
      $("#newWebJarName").val("");
      $("#newWebJarName").parent().removeClass("has-error").removeClass("has-success");
      $("#newWebJarNameFeedback").removeClass("glyphicon-ok").removeClass("glyphicon-remove");
    }

    $("#newWebJarVersion").select2("val", "");
    $("#newWebJarVersion").select2("enable", false);

    $("#deployLog").text("");
  });

});
