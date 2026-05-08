package webjars.views.partials

import java.time.format.DateTimeFormatter
import java.time.LocalDate

object Footer:
  def apply(): String =
    val year = DateTimeFormatter.ofPattern("YYYY").format(LocalDate.now)
    s"""<footer>
    <div class="container d-flex align-items-center justify-content-center footer-content">
        <span>&copy; $year WebJars</span>
    </div>
</footer>"""
