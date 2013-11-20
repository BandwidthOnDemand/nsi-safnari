package controllers

import play.api.Plugin
import play.api.{ Application => App }

class ConnectionManagerPlugin(app: App) extends Plugin {

  lazy val connectionManager = new ConnectionManager(controllers.ConnectionProvider.connectionFactory)

  override def onStart() {}

  override def onStop() {}

}