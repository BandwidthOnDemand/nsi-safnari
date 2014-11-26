package nl.surfnet

import java.net.URI
import nl.surfnet.nsiv2.messages._
import org.joda.time.DateTime
import org.ogf.schemas.nsi._2013._12.connection.types._

package object safnari {
  type RequesterNsa = String
  type ConnectionId = String
  type GlobalReservationId = URI

  private val UuidGenerator = Uuid.randomUuidGenerator

  def newConnectionId: ConnectionId = UuidGenerator().toString

  implicit class ScheduleTypeOps(schedule: ScheduleType) {
    def startTime = Option(schedule.getStartTime).map(_.toDateTime)
    def endTime = Option(schedule.getEndTime).map(_.toDateTime)
  }
}
