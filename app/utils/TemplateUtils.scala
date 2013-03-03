package utils

object TemplateUtils {

  def idify(strings: String*): String  = strings.mkString("-").replace('+', '_')
  
}
