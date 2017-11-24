package utils

import models.WebJarType

class Classic extends WebJarType {
  override val name: String = "Classic"
  override val groupIdQuery: String = "org.webjars"
  override def includesGroupId(groupId: String): Boolean = groupId.equalsIgnoreCase("org.webjars")
}
