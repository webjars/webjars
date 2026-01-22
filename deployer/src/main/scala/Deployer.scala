import zio.*

object Deployer extends ZIOAppDefault:

  def run = ZIO.debug("Hello World!")
