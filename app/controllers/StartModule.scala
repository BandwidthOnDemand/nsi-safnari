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

import akka.actor.*
import anorm.SqlParser.*
import anorm.*
import com.google.inject.{AbstractModule, Provides}
import javax.inject.*
import nl.surfnet.nsiv2.soap.*
import nl.surfnet.safnari.SafnariMessageStore
import play.api.Logger
import play.api.db.Database
import play.api.inject.ApplicationLifecycle
import play.api.mvc.*
import scala.concurrent.*
import scala.concurrent.duration.Duration
import scala.util.control.NonFatal

class StartModule extends AbstractModule:
  override def configure(): Unit =
    bind(classOf[GlobalSettings]).asEagerSingleton()

  @Singleton @Provides def extraBodyParsers(implicit
      ec: ExecutionContext,
      bodyParsers: PlayBodyParsers,
      actionBuilder: DefaultActionBuilder
  ) = new ExtraBodyParsers
  @Singleton @Provides def messageStore(database: Database) = new SafnariMessageStore(database)
  @Singleton @Provides def connectionManager(settings: GlobalSettings): ConnectionManager =
    settings.connectionManager
  @Singleton @Provides def application(
      settings: GlobalSettings,
      configuration: Configuration,
      connectionRequester: ConnectionRequester,
      controllerComponents: ControllerComponents
  )(implicit ec: ExecutionContext): ApplicationController =
    new ApplicationController(
      controllerComponents,
      settings.connectionManager,
      settings.pceRequester,
      connectionRequester,
      configuration
    )
  @Singleton @Provides def discoveryService(
      settings: GlobalSettings,
      configuration: Configuration,
      controllerComponents: ControllerComponents
  )(implicit ec: ExecutionContext): DiscoveryService =
    val discoveryService = new DiscoveryService(settings.pceRequester, configuration)
    discoveryService.setControllerComponents(controllerComponents)
    discoveryService
end StartModule

@Singleton
class GlobalSettings @Inject() (
    lifecycle: ApplicationLifecycle,
    configuration: Configuration,
    actorSystem: ActorSystem,
    pathComputationEngine: PathComputationEngine,
    messageStore: SafnariMessageStore,
    connectionProvider: ConnectionProvider,
    connectionRequester: ConnectionRequester,
    database: Database
)(implicit ec: ExecutionContext):
  private val logger = Logger(classOf[GlobalSettings])

  val pceRequester: ActorRef = pathComputationEngine.createPceRequesterActor(configuration)
  private val createOutboundActor = connectionProvider.outboundActor(
    configuration,
    connectionRequester.nsiRequester,
    pceRequester
  ) _
  val connectionManager: ConnectionManager = new ConnectionManager(
    connectionProvider.connectionFactory(createOutboundActor, configuration),
    configuration,
    messageStore
  )

  if configuration.CleanDbOnStart then cleanDatabase()

  restoreConnectionsFromDatabase()

  lifecycle.addStopHook { () =>
    Future.successful {
      if configuration.CleanDbOnStop then cleanDatabase()
    }
  }

  private def restoreConnectionsFromDatabase(): Unit =
    implicit val implicitActorSystem = actorSystem
    try
      logger.info("Start replaying of connection messages")
      Await.result(connectionManager.restore, Duration.Inf)
      logger.info("Completed replaying of connection messages")
    catch
      case NonFatal(e) =>
        val suppressed = e.getSuppressed()
        suppressed.foreach { e =>
          logger.error(s"Connection replay failed with suppressed exception", e)
        }
        throw e

  private def cleanDatabase(): Unit = database.withTransaction { implicit connection =>
    val tables = SQL(
      "SELECT tablename FROM pg_tables WHERE schemaname = 'public' AND tablename <> 'play_evolutions'"
    ).as(str("tablename").*).map("public." ++ _)
    val truncate = s"TRUNCATE TABLE ${tables.mkString(",")} CASCADE"
    logger.debug(s"Cleaning database: $truncate")
    SQL(truncate).executeUpdate()
    ()
  }
end GlobalSettings
