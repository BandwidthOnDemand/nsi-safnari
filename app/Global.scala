import play.api.GlobalSettings
import play.api.Application
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits._

object Global extends GlobalSettings {

  override def onStart(app: Application) {
    implicit val actorSystem = Akka.system(app)
    controllers.ConnectionProvider.connectionManager.restore
  }
}
