/*
 * Copyright (c) 2012, 2013, 2014, 2015, 2016 SURFnet BV
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the
 * following conditions are met:
 *
 *   * Redistributions of source code must retain the above copyright notice, this list of conditions and the following
 *     disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following
 *     disclaimer in the documentation and/or other materials provided with the distribution.
 *   * Neither the name of the SURFnet BV nor the names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
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
import akka.actor.{ActorRef, Props}
import controllers.PathComputationEngine.DummyPceRequesterActor
import controllers.PathComputationEngine.PceRequesterActor

trait GlobalSettings extends play.api.GlobalSettings {
  private var connectionManager: ConnectionManager = _
  private var pceRequester: ActorRef = _

  override def onStart(app: PlayApp): Unit = {
    pceRequester = createPceRequesterActor(app)
    val createOutboundActor = ConnectionProvider.outboundActor(ConnectionRequester.nsiRequester, pceRequester) _
    connectionManager = new ConnectionManager(ConnectionProvider.connectionFactory(createOutboundActor))(app)
    if (app.configuration.getBoolean("clean.db.on.start") getOrElse false) {
      cleanDatabase(app)
    }

    restoreConnectionsFromDatabase(app)
  }

  def createPceRequesterActor(implicit app: PlayApp): ActorRef =
    app.configuration.getString("pce.actor") match {
      case None | Some("dummy") => Akka.system.actorOf(Props[DummyPceRequesterActor], "pceRequester")
      case _                    => Akka.system.actorOf(Props(new PceRequesterActor(Configuration.PceEndpoint)), "pceRequester")
    }

  override def onStop(app: PlayApp): Unit = {
    if (app.configuration.getBoolean("clean.db.on.stop") getOrElse false) {
      cleanDatabase(app)
    }
  }

  override def getControllerInstance[A](controllerClass: Class[A]): A = {
    (if (controllerClass == classOf[DiscoveryService]) new DiscoveryService(pceRequester)
    else if (controllerClass == classOf[Application]) new Application(connectionManager, pceRequester)
    else controllerClass.getConstructors().head.newInstance(connectionManager)).asInstanceOf[A]
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

  private def cleanDatabase(implicit app: PlayApp): Unit = DB.withTransaction { implicit connection =>
    val tables = SQL("SELECT tablename FROM pg_tables WHERE schemaname = 'public' AND tablename <> 'play_evolutions'").as(str("tablename").*).map("public." ++ _)
    val truncate = s"TRUNCATE TABLE ${tables.mkString(",")} CASCADE"
    Logger.debug(s"Cleaning database: $truncate")
    SQL(truncate).executeUpdate()
    ()
  }
}
