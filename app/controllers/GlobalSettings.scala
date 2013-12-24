package controllers

import anorm._
import anorm.SqlParser._
import play.api.Logger
import play.api.db.DB
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits._
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.control.NonFatal
import play.api.{ Application => PlayApp }

trait GlobalSettings extends play.api.GlobalSettings {
  private var connectionManager: ConnectionManager = _

  override def onStart(app: PlayApp): Unit = {
    connectionManager = new ConnectionManager(ConnectionProvider.connectionFactory)(app)
    if (app.configuration.getBoolean("clean.db.on.start") getOrElse false) {
      cleanDatabase(app)
    }

    restoreConnectionsFromDatabase(app)
  }

  override def onStop(app: PlayApp): Unit = {
    if (app.configuration.getBoolean("clean.db.on.stop") getOrElse false) {
      cleanDatabase(app)
    }
  }

  override def getControllerInstance[A](controllerClass: Class[A]): A = {
    controllerClass.getConstructors().head.newInstance(connectionManager).asInstanceOf[A]
  }

  private def restoreConnectionsFromDatabase(implicit app: PlayApp): Unit = {
    implicit val actorSystem = Akka.system
    try {
      Logger.info("Start replaying of connection messages")
      Await.result(connectionManager.restore, Duration.Inf)
      Logger.info("Completed replaying of connection messages")
    } catch {
      case NonFatal(e) =>
        val suppressed = e.getSuppressed()
        suppressed.foreach { e =>
          Logger.error(s"Connection replay failed with suppressed exception", e)
        }
        throw e
    }
  }

  private def cleanDatabase(implicit app: PlayApp): Unit = {
    DB.withTransaction { implicit connection =>
      val tables = SQL("SELECT tablename FROM pg_tables WHERE schemaname = 'public' AND tablename <> 'play_evolutions'").as(str("tablename").*).map("public." ++ _)
      val truncate = s"TRUNCATE TABLE ${tables.mkString(",")} CASCADE"
      Logger.debug(s"Cleaning database: $truncate")
      SQL(truncate).executeUpdate()
    }
  }
}