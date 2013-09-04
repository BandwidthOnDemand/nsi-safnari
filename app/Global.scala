import anorm._
import anorm.SqlParser._
import play.api.Application
import play.api.GlobalSettings
import play.api.Logger
import play.api.db.DB
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits._

object Global extends GlobalSettings {

  override def onStart(app: Application): Unit = {
    implicit val actorSystem = Akka.system(app)
    if (app.configuration.getBoolean("clean.db.on.start") getOrElse false) {
      cleanDatabase(app)
    }
    controllers.ConnectionProvider.connectionManager.restore
  }

  override def onStop(app: Application): Unit = {
    if (app.configuration.getBoolean("clean.db.on.stop") getOrElse false) {
      cleanDatabase(app)
    }
  }

  private def cleanDatabase(implicit app: Application): Unit = {
    DB.withTransaction { implicit connection =>
      val tables = SQL("SELECT tablename FROM pg_tables WHERE schemaname = 'public' AND tablename <> 'play_evolutions'").as(str("tablename").*).map("public." ++ _)
      val truncate = s"TRUNCATE TABLE ${tables.mkString(",")} CASCADE"
      Logger.debug(s"Cleaning database: $truncate")
      SQL(truncate).executeUpdate()
    }
  }
}
