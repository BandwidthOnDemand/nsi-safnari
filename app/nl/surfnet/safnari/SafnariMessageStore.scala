package nl.surfnet.safnari

import anorm.*
import anorm.SqlParser.*
import java.time.Instant
import nl.surfnet.nsiv2.persistence.MessageStore
import nl.surfnet.nsiv2.utils.*

class SafnariMessageStore(db: play.api.db.Database)
    extends MessageStore[Message](db)(using MessagePersistence.MessageToMessageData):

  def create(
      connectionId: ConnectionId,
      createdAt: Instant,
      requesterNsa: RequesterNsa,
      correlationIdGeneratorVersion: Int
  ): Unit = db.withTransaction { implicit connection =>
    SQL"""
      INSERT INTO connections
             (connection_id,   created_at,                  requester_nsa,   correlation_id_generator_version)
      VALUES (${connectionId}, ${createdAt.toSqlTimestamp}, ${requesterNsa}, ${correlationIdGeneratorVersion})
     """
      .executeInsert()
    ()
  }

  def loadCorrelationIdGeneratorVersions(): Map[ConnectionId, Int] = db.withConnection {
    implicit connection =>
      SQL"""
        SELECT connection_id, correlation_id_generator_version
          FROM connections
         WHERE deleted_at IS NULL AND correlation_id_generator_version IS NOT NULL
       """
        .as((str("connection_id") ~ get[Int]("correlation_id_generator_version")).*)
        .map { case connectionId ~ version => connectionId -> version }
        .toMap
  }
end SafnariMessageStore
