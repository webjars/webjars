package models

trait WebJarType {
  val name: String
  val groupIdQuery: String
  def includesGroupId(groupId: String): Boolean
  override def toString: String = name
}

object WebJarType {

  def fromGroupId[A <: WebJarType](groupId: String, webJarTypes: Set[A]): Option[A] = webJarTypes.find(_.includesGroupId(groupId))
  def fromString[A <: WebJarType](s: String, webJarTypes: Set[A]): Option[A] = webJarTypes.find(_.name.equalsIgnoreCase(s))
  def toString(webJarType: WebJarType): String = webJarType.name.toLowerCase

}
