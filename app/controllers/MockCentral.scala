package controllers

import play.api.mvc.InjectedController

class MockCentral extends InjectedController {

  def searchGroup = Action {
    Ok(
      """
        |{
        |  "responseHeader": {
        |    "status":0,
        |    "QTime":0,
        |    "params": {"fl":"id,g,a,v,p,ec,timestamp,tags","sort":"score desc,g asc,a asc,v desc","indent":"off","q":"g:\"org.webjars\"","core":"gav","wt":"json","rows":"20","version":"2.2"}
        |  },
        |  "response": {
        |    "numFound":3,
        |    "start":0,
        |    "docs": [
        |      {"id":"org.webjars:bootstrap:2.1.1","g":"org.webjars","a":"bootstrap","v":"2.1.1","p":"jar","timestamp":1350423135000,"tags":["bootstrap","webjar"],"ec":["-sources.jar",".jar",".pom"]},
        |      {"id":"org.webjars:bootstrap:2.1.0","g":"org.webjars","a":"bootstrap","v":"2.1.0","p":"jar","timestamp":1350423135000,"tags":["bootstrap","webjar"],"ec":["-sources.jar",".jar",".pom"]},
        |      {"id":"org.webjars:jquery:1.8.2","g":"org.webjars","a":"jquery","v":"1.8.2","p":"jar","timestamp":1350320427000,"tags":["jquery","webjar"],"ec":[".jar",".pom"]},
        |      {"id":"org.webjars:jquery-ui:1.9.0","g":"org.webjars","a":"jquery-ui","v":"1.9.0","p":"jar","timestamp":1350490675000,"tags":["jquery","webjar"],"ec":["-javadoc.jar",".jar",".pom"]}
        |    ]
        |  }
        |}
      """.stripMargin)
  }

}
