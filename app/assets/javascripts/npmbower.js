$(function() {

  function getPackageOrRepoName() {
    var packageOrUrl = $("#newWebJarName").val().split("#");

    var data = {};

    if (packageOrUrl.length > 0) {
      data.packageOrRepo = packageOrUrl[0];
    }

    if (packageOrUrl.length == 2) {
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
      url: "/_" + webJarType + "/exists?name=" + packageName,
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

      var url = "/_" + webJarType + "/versions?name=" + packageOrRepoName.packageOrRepo;

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

    var deployUrl = "/_" + webJarType + "/deploy?name=" + encodeURIComponent(artifactId) + "&version=" + encodeURIComponent(version) + "&channelId=" + channelId;

    $.ajax(deployUrl, {
      method: "post",
      success: function(data) {
        console.log(data);
      }
    });
  });

  $('#newWebJarModal').on('show.bs.modal', function (event) {
    var artifactId = $(event.relatedTarget).data('artifact-id');
    if (artifactId != undefined) {
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
