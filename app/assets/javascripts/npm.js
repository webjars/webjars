$(function() {

  $("#newNPMWebJarName").typeWatch({
    callback: function(packageName) {
      $("#newNPMWebJarName").parent().removeClass("has-error").removeClass("has-success");
      $("#newNPMWebJarNameFeedback").removeClass("glyphicon-ok").removeClass("glyphicon-remove");

      $.ajax({
        url: "/_npm/exists/" + packageName,
        success: function(data, status) {
          $("#newNPMWebJarName").parent().addClass("has-success");
          $("#newNPMWebJarNameFeedback").addClass("glyphicon-ok").removeClass("hidden");
          $("#newNPMWebJarVersion").select2("enable", true);
        },
        error: function(data, status) {
          $("#newNPMWebJarName").parent().addClass("has-error");
          $("#newNPMWebJarNameFeedback").addClass("glyphicon-remove").removeClass("hidden");
          $("#newNPMWebJarVersion").select2("enable", false);
        }
      });
    },
    wait: 750,
    captureLength: 0
  });

  $("#newNPMWebJarVersion").select2({
    placeholder: "Version",
    id: function(item) {
      return item;
    },
    query: function(query) {
      var name = $("#newNPMWebJarName").val();

      $.getJSON("/_npm/versions/" + name, function(data) {
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
    $("#deployNPMButton").attr("disabled", false);
  });

  $("#deployNPMButton").click(function(event) {
    event.preventDefault();

    var deployLog = $("#deployLog");
    function log(message) {
      var t = deployLog.text();
      deployLog.text(message + "\n" + t);
    }

    deployLog.addClass("show");
    deployLog.removeClass("hidden");

    $("#deployNPMButton").attr("disabled", true);

    var artifactId = $("#newNPMWebJarName").val();
    var version = $("#newNPMWebJarVersion").select2("val");

    var pusher = new Pusher(window.pusherKey);
    var channelId = guid();
    var channel = pusher.subscribe(channelId);
    channel.bind("update", function(data) {
      log(data);
    });
    channel.bind("success", function(data) {
      log(data);
      pusher.disconnect();
      $("#deployNPMButton").attr("disabled", false);
    });
    channel.bind("failure", function(data) {
      log(data);
      pusher.disconnect();
      $("#deployNPMButton").attr("disabled", false);
    });

    deployLog.text("Starting Deploy");

    $.ajax("/deploy/npm/" + artifactId + "/" + version + "?channelId=" + channelId, {
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