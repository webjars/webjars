$(function() {

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