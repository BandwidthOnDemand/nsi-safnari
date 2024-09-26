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

import akka.actor._
import akka.event.LoggingReceive
import java.time.Instant
import java.util.concurrent.TimeoutException
import javax.inject._
import nl.surfnet.nsiv2.messages._
import nl.surfnet.nsiv2.soap._
import nl.surfnet.safnari._
import org.ogf.schemas.nsi._2013._12.connection.types.ReservationConfirmCriteriaType
import play.api.Logger
import play.api.mvc._
import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Success }

import controllers.ActorSupport._


@Singleton
class ConnectionRequesterController @Inject()(connectionManager: ConnectionManager, configuration: Configuration, connectionRequester: ConnectionRequester, extraBodyParsers: ExtraBodyParsers, val actionBuilder: DefaultActionBuilder)(implicit ec: ExecutionContext) extends InjectedController with SoapWebService {

  override val WsdlRoot = "wsdl/2.0"
  override val WsdlPath = ""
  override val WsdlBasename = "ogf_nsi_connection_requester_v2_0.wsdl"

  override def serviceUrl: String = configuration.requesterServiceUrl

  def request = extraBodyParsers.NsiRequesterEndPoint(configuration.NsaId) {
    case message @ NsiRequesterMessage(headers, notification: NsiNotification) =>
      val connection = connectionManager.findByChildConnectionId(notification.connectionId)

      val ack = connection.map { c =>
        (c ? Connection.Command(Instant.now(), FromProvider(NsiRequesterMessage(headers, notification))))
      } getOrElse Future.successful(ServiceException(NsiError.ReservationNonExistent.toServiceException(configuration.NsaId)))

      ack.map(message.ack)
    case response =>
      connectionRequester.handleResponse(response)
      // FIXME return error when message cannot be handled?
      Future.successful(response.ack(GenericAck()))
  }

}

@Singleton
class ConnectionRequester @Inject()(configuration: Configuration, nsiWebService: NsiWebService)(implicit actorSystem: ActorSystem, ec: ExecutionContext) {
  private val logger = Logger(classOf[ConnectionRequester])

  private val continuations = new Continuations[NsiRequesterMessage[NsiRequesterOperation]](actorSystem.scheduler)

  private[controllers] def handleResponse(message: NsiRequesterMessage[NsiRequesterOperation]): Unit =
    continuations.replyReceived(message.headers.correlationId, message)

  def nsiRequester: ActorRef =
    configuration.NsiActor match {
      case None | Some("dummy") => actorSystem.actorOf(Props(new DummyNsiRequesterActor))
      case _                    => actorSystem.actorOf(Props(new NsiRequesterActor(configuration)))
    }

  class NsiRequesterActor(configuration: Configuration) extends Actor {
    private val uuidGenerator = Uuid.randomUuidGenerator()
    private def newCorrelationId() = CorrelationId.fromUuid(uuidGenerator())

    def receive = {
      case 'healthCheck =>
        sender ! Future.successful("NSI requester (Real)" -> true)

      case ToProvider(message @ NsiProviderMessage(headers, operation: NsiProviderOperation), provider) =>
        val connectionId = operation match {
          case command: NsiProviderCommand => command.optionalConnectionId
          case _ => None
        }

        val connection = Connection(sender)

        continuations.register(headers.correlationId, configuration.ConnectionExpirationTime).foreach { reply =>
          connection ! Connection.Command(Instant.now(), FromProvider(reply))
        }
        continuations.addTimeout(headers.correlationId, configuration.AsyncReplyTimeout) {
          connection ! Connection.Command(
              Instant.now(),
              MessageDeliveryFailure(newCorrelationId(), connectionId, headers.correlationId, provider.url, Instant.now(), s"No reply received within: ${configuration.AsyncReplyTimeout}"))
        }

        val response = nsiWebService.callProvider(provider, message, configuration)
        response.onComplete {
          case Failure(timeout: TimeoutException) =>
            // Let the requester timeout as well (or receive an actual reply). No need to send an ack timeout!
          case Failure(exception) =>
            logger.warn(s"communication failure calling $provider", exception)
            continuations.unregister(headers.correlationId)
            connection ! Connection.Command(Instant.now(), MessageDeliveryFailure(newCorrelationId(), connectionId, headers.correlationId, provider.url, Instant.now(), exception.toString))
          case Success(ack @ NsiProviderMessage(_, ServiceException(_))) =>
            continuations.unregister(headers.correlationId)
            connection ! Connection.Command(Instant.now(), AckFromProvider(ack))
          case Success(ack) =>
            connection ! Connection.Command(Instant.now(), AckFromProvider(ack))
        }
    }
  }

  class DummyNsiRequesterActor extends Actor {
    private var connectionCriteria: Map[ConnectionId, ReservationConfirmCriteriaType] = Map.empty

    def receive = LoggingReceive {
      case 'healthCheck =>
        sender ! Future.successful("NSI requester (Dummy)" -> true)
      case ToProvider(message @ NsiProviderMessage(headers, reserve: InitialReserve), _) =>
        val connectionId = newConnectionId

        val requestCriteria = reserve.body.getCriteria

        val confirmCriteria = new ReservationConfirmCriteriaType()
          .withSchedule(requestCriteria.getSchedule)
          .withServiceType(requestCriteria.getServiceType)
          .withVersion(if (requestCriteria.getVersion eq null) 1 else requestCriteria.getVersion)
          .withAny(requestCriteria.getAny)

        requestCriteria.pointToPointService.foreach { p2ps =>
          val qualified = p2ps.shallowCopy
          qualified.setSourceSTP(qualifyStp(p2ps.getSourceSTP))
          qualified.setDestSTP(qualifyStp(p2ps.getDestSTP))
          confirmCriteria.withPointToPointService(qualified)
        }

        connectionCriteria += connectionId -> confirmCriteria

        Connection(sender) ! Connection.Command(Instant.now(), AckFromProvider(message ack ReserveResponse(connectionId)))
        Connection(sender) ! Connection.Command(Instant.now(), FromProvider(message reply ReserveConfirmed(connectionId, confirmCriteria)))

      case ToProvider(message @ NsiProviderMessage(headers, reserve: ModifyReserve), _) =>
        connectionCriteria.get(reserve.connectionId) map { criteria =>
          reserve.body.getCriteria.modifiedCapacity.foreach { capacity => criteria.pointToPointService.foreach { p2ps => p2ps.setCapacity(capacity) } }

          Connection(sender) ! Connection.Command(Instant.now(), AckFromProvider(message ack ReserveResponse(reserve.connectionId)))
          Connection(sender) ! Connection.Command(Instant.now(), FromProvider(message reply ReserveConfirmed(reserve.connectionId, criteria)))
        } getOrElse {
          Connection(sender) ! Connection.Command(Instant.now(), AckFromProvider(message ack ServiceException(NsiError.ReservationNonExistent.toServiceException(headers.providerNSA))))
        }

      case ToProvider(message @ NsiProviderMessage(headers, commit: ReserveCommit), _) =>
        Connection(sender) ! Connection.Command(Instant.now(), AckFromProvider(message ack GenericAck()))
        Connection(sender) ! Connection.Command(Instant.now(), FromProvider(message reply ReserveCommitConfirmed(commit.connectionId)))
      case ToProvider(message @ NsiProviderMessage(headers, provision: Provision), _) =>
        Connection(sender) ! Connection.Command(Instant.now(), AckFromProvider(message ack GenericAck()))
        Connection(sender) ! Connection.Command(Instant.now(), FromProvider(message reply ProvisionConfirmed(provision.connectionId)))
      case ToProvider(message @ NsiProviderMessage(headers, terminate: Terminate), _) =>
        Connection(sender) ! Connection.Command(Instant.now(), AckFromProvider(message ack GenericAck()))
        Connection(sender) ! Connection.Command(Instant.now(), FromProvider(message reply TerminateConfirmed(terminate.connectionId)))
      case ToProvider(message @ NsiProviderMessage(headers, update: NsiProviderUpdateCommand), provider) =>
        Connection(sender) ! Connection.Command(Instant.now(), AckFromProvider(message ack ServiceException(NsiError.NotImplemented.toServiceException(provider.nsa).withConnectionId(update.connectionId))))
      case ToProvider(message @ NsiProviderMessage(headers, query: QueryRecursive), provider) =>
        Connection(sender) ! Connection.Command(Instant.now(), AckFromProvider(message ack ServiceException(NsiError.NotImplemented.toServiceException(provider.nsa))))
      case ToProvider(message @ NsiProviderMessage(headers, query: NsiProviderQuery), provider) =>
        Connection(sender) ! Connection.Command(Instant.now(), AckFromProvider(message ack ServiceException(NsiError.NotImplemented.toServiceException(provider.nsa))))
    }

    private def qualifyStp(s: String): String = Stp.fromString(s).map { stp =>
      stp.vlan match {
        case None => stp.toString
        case Some(vlanRange) => stp.withLabel("vlan", vlanRange.lowerBound.toString).toString
      }
    }.getOrElse(s)
  }

}
