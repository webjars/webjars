function setupWebJarList() {
  // list files modal
  $(".file-list-link").click(onFileList);

  // build tool change handler
  $("#buildtoolselect").find("input").click(function (e) {
      // update for each webjar
      buildTool = $(this).attr("value");
      updateAllDetails(buildTool);
    });
}

function searchWebJars(query, groupIds) {
  var groupIdsQueryString = groupIds.reduce(function (acc, val) {
    acc += `&groupId=${val}`;
    return acc;
  }, "");
  $.get(`/search?query=${query}${groupIdsQueryString}`, function (data) {
    $("#listTitle").html("Search Results");

    $("#webJarList").html(data);

    setupWebJarList();
  });
}

function onFileList(event) {
  // allow middle clicks to open in a new tab
  if (event.button === 0 && !event.metaKey) {
    event.preventDefault();
    $("#fileListModalLabel").text(`Files for ${$(this).parents("tr").data("artifact")}`);
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
      instructions = `'${groupId}:${artifactId}:jar:${webJarVersion.val()}'`;
      break;
    case "gradle":
      instructions = `runtimeOnly("${groupId}:${artifactId}:${webJarVersion.val()}")`;
      break;
    case "grape":
      instructions = `@Grapes(
    @Grab(group='${groupId}', module='${artifactId}', version='webJarVersion.val()')
)`;
      break;
    case "ivy":
      instructions = `<dependency org="${groupId}" name="${artifactId}" rev="${webJarVersion.val()}" />`;
      break;
    case "leiningen":
      instructions = `${groupId}/${artifactId} "${webJarVersion.val()}"`;
      break;
    case "maven":
      instructions = `<dependency>
    <groupId>${groupId}</groupId>
    <artifactId>${artifactId}</artifactId>
    <version>${webJarVersion.val()}</version>
</dependency>`;
      break;
    case "sbt":
      instructions = `"${groupId}" % "${artifactId}" % "${webJarVersion.val()}"`;
      break;
  }

  row.find(".build-instructions pre").text(instructions);

  // update files link
  var filesLink = $("<a>").attr("href", `/listfiles/${groupId}/${artifactId}/${webJarVersion.val()}`).addClass("file-list-link");
  filesLink.click(onFileList);
  filesLink.text(`${webJarVersion.find(":selected").data("numfiles")} Files`);
  row.find(".files").empty().append(filesLink);
}

function updateAllDetails(buildTool) {
  $("tr[data-artifact]").each(function () {
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

  return `${s4()}${s4()}-${s4()}-${s4()}-${s4()}-${s4()}${s4()}${s4()}`;
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
  $("#newWebJarName").removeClass("is-valid is-invalid");
  $("#newWebJarNameSpinner").addClass("spinner-border");
  $("#newWebJarVersion").val("").trigger("change");
  $("#newWebJarVersion").prop("disabled", true);
  $("#classicDeployError").addClass("d-none");

  $.ajax({
    url: `/exists?webJarType=${webJarType()}&name=${packageName}`,
    success: function (data, status) {
      $("#newWebJarName").addClass("is-valid");
      $("#newWebJarNameSpinner").removeClass("spinner-border");
      $("#newWebJarVersion").prop("disabled", false);
    },
    error: function (data, status) {
      if (webJarType() === "classic") {
        $("#classicErrorUrl").attr("href", `https://github.com/webjars/${packageName}/issues/new`);
        $("#classicDeployError").removeClass("d-none");
      }
      $("#newWebJarName").addClass("is-invalid");
      $("#newWebJarNameSpinner").removeClass("spinner-border");
      $("#newWebJarVersion").prop("disabled", true);
    },
  });
}

function handleSearch() {
  var searchText = $("#search").val();
  var groupIds = $("input[name='search_catalog[]']:checked")
    .map(function () {
      return $(this).val();
    })
    .get();

  if (searchText === "") {
    $("#clearSearch").hide();
  } else {
    $("#clearSearch").show();
    searchWebJars(searchText, groupIds);
  }
}

$(function () {
  setupWebJarList();

  $("#search").keyup(function (event) {
    if (event.keyCode === 13 || this.value.length > 2) {
      handleSearch();
    }
  });

  $("input[name='search_catalog[]']").change(function () {
    handleSearch();
  });

  $("#clearSearch").click(function () {
    $("#search").val("");
    $(this).hide();

    $.get("/popular", function (data) {
      $("#listTitle").html("Popular WebJars");

      $("#webJarList").html(data);

      setupWebJarList();
    });
  });

  $("#newWebJarName").typeWatch({
    callback: checkPackageName,
    wait: 600,
    captureLength: 0,
  });

  $("#newWebJarVersion").select2({
      dropdownParent: $("#newWebJarVersion").parent(),
      theme: "bootstrap-5",
      placeholder: $(this).data("placeholder"),
      ajax: {
        url: function() {
          var packageOrRepoName = getPackageOrRepoName();

          var url = `/versions?webJarType=${webJarType()}&name=${packageOrRepoName.packageOrRepo}`;

          if (packageOrRepoName.branch !== undefined) {
            url += `&branch=${packageOrRepoName.branch}`;
          }

          return url;
        },
        dataType: "json",
        delay: 250,
        processResults: function(data, params) {
          if (!!params.term) {
            data = data.filter(item => item.startsWith(params.term));
          }

          const results = data.map(item => ({ id: item, text: item }));

          return { results };
        }
      },
    })
    .on("select2:select", function () {
      $("#deployButton").attr("disabled", false);
    })
    .val("");

  $("#deployButton").click(function (event) {
    event.preventDefault();

    var deployLog = $("#deployLog");
    function log(message) {
      deployLog.append(document.createTextNode(message));
      deployLog.animate({ scrollTop: deployLog.prop("scrollHeight") }, 0);
    }

    $("#deployButton").attr("disabled", true);

    var packageOrRepoName = getPackageOrRepoName();

    var artifactId = packageOrRepoName.packageOrRepo;
    var version = $("#newWebJarVersion").val();

    deployLog.text("Starting Deploy\n");

    var deployUrl = `/deploy?webJarType=${webJarType()}&nameOrUrlish=${encodeURIComponent(artifactId)}&version=${encodeURIComponent(version)}`;
    var source = new EventSource(deployUrl);

    source.addEventListener("message", function (e) {
      if (e.data.length > 0) {
        var message = e.data;
        if (!e.data.endsWith("\n")) {
          message = message + "\n";
        }
        log(message);
      }
    });
    source.addEventListener("error", function (e) {
      source.close();
      $("#deployButton").attr("disabled", false);
    });
  });

  $("#newWebJarModal").on("show.bs.modal", function (event) {
    $("#deployButton").attr("disabled", true);
    $("input[type=radio][name=new_webjar_catalog]:checked").trigger("change");

    var groupId = $(event.relatedTarget).data("group-id");
    var artifactId = $(event.relatedTarget).data("artifact-id");
    var name = $(event.relatedTarget).data("name");
    var input = undefined;

    $("input[type=radio][name=new_webjar_catalog]").prop("checked", false);

    if (groupId === "org.webjars") {
      $("input[type=radio][name=new_webjar_catalog][value='classic']").prop("checked", true).trigger("change");
      input = artifactId;
    }
    else if (groupId === "org.webjars.npm") {
      $("input[type=radio][name=new_webjar_catalog][value='npm']").prop("checked", true).trigger("change");
      input = name;
    }

    if (input !== undefined) {
      $("#newWebJarName").val(input);
      checkPackageName(input);
    } else {
      $("#newWebJarName").val("");
      $("#newWebJarName").removeClass("is-valid").removeClass("is-invalid");
    }

    $("#newWebJarVersion").val("").trigger("change");
    $("#newWebJarVersion").prop("disabled", true);

    $("#deployLog").text("");
  });
});

function cometMessage(event) {
  console.log("Received event: " + event);
}
