$(function() {

  $("#newWebJarName").typeWatch({
    callback: function(packageName) {
      $("#newWebJarName").parent().removeClass("has-error").removeClass("has-success");
      $("#newWebJarNameFeedback").removeClass("glyphicon-ok").removeClass("glyphicon-remove");

      $.ajax({
        url: "/_" + webJarType + "/exists/" + packageName,
        success: function(data, status) {
          $("#newWebJarName").parent().addClass("has-success");
          $("#newWebJarNameFeedback").addClass("glyphicon-ok").removeClass("hidden");
          $("#newWebJarVersion").select2("enable", true);
        },
        error: function(data, status) {
          $("#newWebJarName").parent().addClass("has-error");
          $("#newWebJarNameFeedback").addClass("glyphicon-remove").removeClass("hidden");
          $("#newWebJarVersion").select2("enable", false);
        }
      });
    },
    wait: 750,
    captureLength: 0
  });

  $("#newWebJarVersion").select2({
    placeholder: "Version",
    id: function(item) {
      return item;
    },
    query: function(query) {
      var name = $("#newWebJarName").val();

      $.getJSON("/_" + webJarType + "/versions/" + name, function(data) {
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

    deployLog.addClass("show");
    deployLog.removeClass("hidden");

    $("#deployButton").attr("disabled", true);

    var artifactId = $("#newWebJarName").val();
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

    $.ajax("/deploy/" + webJarType + "/" + artifactId + "/" + version + "?channelId=" + channelId, {
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