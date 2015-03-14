var webjars = [];

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
          if( $(this).data("artifact").indexOf(search) == -1 ) {
              $(this).hide();
          }
          else {
              $(this).show()
          }
      });
  };

  $("#newBowerWebJarName").select2({
    placeholder: "Bower Package Name",
    ajax: {
      url: "/_bower/packages",
      dataType: "json",
      quietMillis: 250,
      data: function (term, page) {
        return {
          query: term
        };
      },
      results: function (data, page) {
        return {results: data};
      },
      cache: true
    },
    initSelection: function(element, callback) {
      var id = $(element).val();
      if (id !== "") {
        console.log("init");
        //$.ajax("https://api.github.com/repositories/" + id, {
        //  dataType: "json"
        //}).done(function(data) { callback(data); });
      }
    },
    formatResult: function(item) {
      return item.name;
    }
  });

});

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