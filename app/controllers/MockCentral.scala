package controllers

import play.api.mvc.{Action, Controller}
import play.api.libs.json.Json

object MockCentral extends Controller {
  
  def searchGroup = Action {
    
    val jsonObject = Json.toJson(
      Map(
        "response" -> Json.toJson(
          Map(
            "docs" -> Seq(
              Json.toJson(
                Map(
                  "a" -> Json.toJson("jquery"),
                  "versionCount" -> Json.toJson(1),
                  "latestVersion" -> Json.toJson("1.8.2")
                )
              ),
              Json.toJson(
                Map(
                  "a" -> Json.toJson("bootstrap"),
                  "versionCount" -> Json.toJson(1),
                  "latestVersion" -> Json.toJson("2.1.1")
                )
              ),
              Json.toJson(
                Map(
                  "a" -> Json.toJson("jquery-ui"),
                  "versionCount" -> Json.toJson(2),
                  "latestVersion" -> Json.toJson("1.3")
                )
              )
            )
          )
        )
      )
    )
    
    Ok(jsonObject)
  }

}
