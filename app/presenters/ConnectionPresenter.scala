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
package presenters

import nl.surfnet.nsiv2.messages.{given, *}
import nl.surfnet.nsiv2.utils.*
import java.time.Instant
import org.ogf.schemas.nsi._2013._12.connection.types.{
  QuerySummaryResultType,
  ReservationRequestCriteriaType,
  ScheduleType
}
import scala.jdk.CollectionConverters.*
import org.ogf.schemas.nsi._2013._12.connection.types.QuerySummaryResultCriteriaType

case class ConnectionPresenter(
    private val data: QuerySummaryResultType,
    val pendingCriteria: Option[ReservationRequestCriteriaType]
):
  private val statusPresenter = Nsi2StatusPresenter(data.getConnectionStates)

  def connectionId: ConnectionId = data.getConnectionId
  def globalReservationId: Option[String] =
    Option(data.getGlobalReservationId).map(_.trim).filter(_.nonEmpty)
  def description: Option[String] = Option(data.getDescription).map(_.trim).filter(_.nonEmpty)
  def requesterNsa = data.getRequesterNSA
  def status = statusPresenter.status
  def dataPlaneStatus: String =
    if data.getConnectionStates.getDataPlaneStatus.isActive then "active" else "inactive"

  def committedCriteria: Option[QuerySummaryResultCriteriaType] =
    if data.getCriteria.isEmpty then None else Some(data.getCriteria.asScala.maxBy(_.getVersion))

  private val schedule = committedCriteria
    .map(_.getSchedule)
    .orElse(pendingCriteria.map(_.getSchedule))
    .getOrElse(new ScheduleType())
  private val pointToPointService = committedCriteria
    .flatMap(_.pointToPointService)
    .orElse(pendingCriteria.flatMap(_.pointToPointService))

  def startTime = schedule.startTime
  def endTime = schedule.endTime
  def bandwidth: Option[Long] = pointToPointService.map(_.getCapacity)
  def sourceStp: Option[String] = pointToPointService.map(_.getSourceSTP)
  def destinationStp: Option[String] = pointToPointService.map(_.getDestSTP)

  def committedVersion: Option[Int] = committedCriteria.map(_.getVersion)
  def pendingVersion: Option[Int] =
    pendingCriteria.map(_.version orElse (committedVersion.map(_ + 1)) getOrElse 1)

  def qualifier(now: Instant): String =
    def inFuture(dt: Instant) = dt.isAfter(now)

    if startTime.fold2(inFuture, false, false) then "future"
    else if endTime.fold2(inFuture, true, true) then "current"
    else "past"
end ConnectionPresenter
