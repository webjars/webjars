package webjars.views.partials

import zio.http.template2.*

import java.time.LocalDate
import java.time.format.DateTimeFormatter

object Footer:
  def apply(): Dom =
    val year = DateTimeFormatter.ofPattern("YYYY").format(LocalDate.now)
    footer(
      div(className := "container d-flex align-items-center justify-content-center footer-content",
        span(Dom.raw(s"&copy; $year WebJars"))
      )
    )
