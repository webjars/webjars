var webjars = [];
var buildTool = "sbt" ;

$(function() {
  $.getJSON("/list/org.webjars.bower", function(data) {
    webjars = data;

    $("tr").each(function (i, tr) {
      var webjar = getWebjar($(tr).data("artifact"));
      if (webjar != undefined) {
        webjar['row'] = $(tr);
      }
    });
  }).fail(function() {
    console.log("error");
  });

  $("#buildtoolselect").find("label").click(function(e) {
    // update for each webjar
    buildTool = $(this).attr("data-value");
    $.each(webjars, function(i, webjar) { updateDetails(webjar); });
  });

  /**
   * artifact search functionality
   */
   $("#search").keyup(function(a,b,c){
      var searchText = $(this).val();
      if(searchText == ""){
          $("#clearSearch").hide();
      }
      else {
          $("#clearSearch").show();
      }
      if(searchText.length == 1){
          return;
      }
      filter(searchText)
  });

  $("#clearSearch").click(function(){
      $("#search").val("");
      $(this).hide();
      filter();
  });

  var filter = function(search){
      if(!search){
          $("tr[data-artifact]").show();
          return;
      }
      $("tr[data-artifact]").each(function(){
          if( $(this).data("artifact").toLowerCase().indexOf(search.toLowerCase()) == -1 ) {
              $(this).hide();
          }
          else {
              $(this).show()
          }
      });
  };

  function formatBowerPackageName(item) {
    return item.name + " (" + item.website.split("://")[1] + ")";
  }

  $("#newBowerWebJarName").select2({
    placeholder: "Bower Package Name",
    id: function(item) {
      return item.name;
    },
    ajax: {
      url: "/_bower/packages",
      dataType: "json",
      quietMillis: 250,
      data: function (term, page) {
        return {
          query: term,
          page: page || 1
        };
      },
      results: function (data, page) {
        return {
          results: data.results,
          more: (page * 30) < data.total_count
        };
      },
      cache: true
    },
    formatResult: formatBowerPackageName,
    formatSelection: formatBowerPackageName
  }).on("change", function(event) {
    $("#newBowerWebJarVersion").select2("enable", true);
    $("#newBowerWebJarVersion").select2("data", []);
  });

  $("#newBowerWebJarVersion").select2({
    placeholder: "Version",
    id: function(item) {
      return item;
    },
    query: function(query) {
      var name = $("#newBowerWebJarName").select2("val");

      $.getJSON("/_bower/versions/" + name, function(data) {
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
    $("#deployBowerButton").attr("disabled", false);
  });

  $("#deployBowerButton").click(function(event) {
    event.preventDefault();

    var deployLog = $("#deployLog");
    function log(message) {
      var t = deployLog.text();
      deployLog.text(message + "\n" + t);
    }

    deployLog.addClass("show");
    deployLog.removeClass("hidden");

    $("#deployBowerButton").attr("disabled", true);

    var artifactId = $("#newBowerWebJarName").select2("val");
    var version = $("#newBowerWebJarVersion").select2("val");

    var pusher = new Pusher(window.pusherKey);
    var channelId = guid();
    var channel = pusher.subscribe(channelId);
    channel.bind("update", function(data) {
      log(data);
    });
    channel.bind("success", function(data) {
      log(data);
      pusher.disconnect();
      $("#deployBowerButton").attr("disabled", false);
    });
    channel.bind("failure", function(data) {
      log(data);
      pusher.disconnect();
      $("#deployBowerButton").attr("disabled", false);
    });

    deployLog.text("Starting Deploy");

    $.ajax("/deploy/bower/" + artifactId + "/" + version + "?channelId=" + channelId, {
      method: "post",
      success: function(data) {
        console.log(data);
      }
    });
  });

});

// from: http://stackoverflow.com/a/105074/77409
function guid() {
  function s4() {
    return Math.floor((1 + Math.random()) * 0x10000)
      .toString(16)
      .substring(1);
  }
  return s4() + s4() + '-' + s4() + '-' + s4() + '-' +
    s4() + '-' + s4() + s4() + s4();
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
      instructions = "'" + webjar.groupId + ":" + webjar.artifactId + ":jar:" + webjarVersion + "'";
      break;
    case "gradle":
      instructions = "compile '" + webjar.groupId + ":" + webjar.artifactId + ":" + webjarVersion + "'";
      break;
    case "grape":
      instructions = "@Grapes(\n" +
        "    @Grab(group='" + webjar.groupId + "', module='" + webjar.artifactId + "', version='" + webjarVersion + "')\n" +
        ")";
      break;
    case "ivy":
      instructions = '<dependency org="' + webjar.groupId + '" name="' + webjar.artifactId + '" rev="' + webjarVersion + '" />';
      break;
    case "leiningen":
      instructions = webjar.groupId + "/" + webjar.artifactId + ' "' + webjarVersion + '"';
      break;
    case "maven":
      instructions = "<dependency>\n" +
        "    <groupId>" + webjar.groupId + "</groupId>\n" +
        "    <artifactId>" + webjar.artifactId + "</artifactId>\n" +
        "    <version>" + webjarVersion + "</version>\n" +
        "</dependency>";
      break;
    case "sbt":
      instructions = '"' + webjar.groupId + '" % "' + webjar.artifactId + '" % "' + webjarVersion + '"';
      break;
  }

  row(webjar.artifactId).find(".build-instructions pre").text(instructions);
}