/*
 * Copyright (c) 2012, 2013, 2014, 2015 SURFnet BV
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
import akka.pattern.ask
import java.net.URI
import java.util.concurrent.TimeoutException
import nl.surfnet.nsiv2._
import nl.surfnet.nsiv2.messages._
import soap._
import soap.ExtraBodyParsers._
import nl.surfnet.safnari._
import org.joda.time.DateTime
import org.joda.time.Instant
import play.api.Logger
import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc.Controller
import scala.concurrent.Future
import scala.util.{ Failure, Success }

import controllers.ActorSupport._


class ConnectionRequester(connectionManager: ConnectionManager) extends Controller with SoapWebService {

  override val WsdlRoot = "wsdl/2.0"
  override val WsdlPath = ""
  override val WsdlBasename = "ogf_nsi_connection_requester_v2_0.wsdl"

  override def serviceUrl: String = ConnectionRequester.serviceUrl

  def request = NsiRequesterEndPoint(Configuration.NsaId) {
    case message @ NsiRequesterMessage(headers, notification: NsiNotification) =>
      val connection = connectionManager.findByChildConnectionId(notification.connectionId)

      val ack = connection.map { c =>
        (c ? Connection.Command(new Instant(), FromProvider(NsiRequesterMessage(headers, notification))))
      } getOrElse Future.successful(ServiceException(NsiError.ConnectionNonExistent.toServiceException(Configuration.NsaId)))

      ack.map(message.ack)
    case response =>
      ConnectionRequester.continuations.replyReceived(response.headers.correlationId, response)
      // FIXME return error when message cannot be handled?
      Future.successful(response.ack(GenericAck()))
  }

}

object ConnectionRequester {
  def serviceUrl: String = s"${Configuration.BaseUrl}${routes.ConnectionRequester.request().url}"
  val continuations = new Continuations[NsiRequesterMessage[NsiRequesterOperation]](actorSystem.scheduler)

  def nsiRequester: ActorRef =
    current.configuration.getString("nsi.actor") match {
      case None | Some("dummy") => actorSystem.actorOf(Props[DummyNsiRequesterActor])
      case _                    => actorSystem.actorOf(Props(new NsiRequesterActor(Configuration.NsaId, URI.create(serviceUrl))))
    }

  class NsiRequesterActor(requesterNsa: String, requesterUrl: URI) extends Actor {
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

        continuations.register(headers.correlationId, Configuration.ConnectionExpirationTime).foreach { reply =>
          connection ! Connection.Command(new Instant(), FromProvider(reply))
        }
        continuations.addTimeout(headers.correlationId, Configuration.AsyncReplyTimeout) {
          connection ! Connection.Command(
              new Instant(),
              MessageDeliveryFailure(newCorrelationId(), connectionId, headers.correlationId, provider.url, DateTime.now(), s"No reply received within: ${Configuration.AsyncReplyTimeout}"))
        }

        val response = NsiWebService.callProvider(provider, message)
        response.onComplete {
          case Failure(timeout: TimeoutException) =>
            // Let the requester timeout as well (or receive an actual reply). No need to send an ack timeout!
          case Failure(exception) =>
            Logger.warn(s"communication failure calling $provider", exception)
            continuations.unregister(headers.correlationId)
            connection ! Connection.Command(new Instant(), MessageDeliveryFailure(newCorrelationId(), connectionId, headers.correlationId, provider.url, DateTime.now(), exception.toString))
          case Success(ack @ NsiProviderMessage(_, ServiceException(_))) =>
            continuations.unregister(headers.correlationId)
            connection ! Connection.Command(new Instant(), AckFromProvider(ack))
          case Success(ack) =>
            connection ! Connection.Command(new Instant(), AckFromProvider(ack))
        }
    }
  }

  class DummyNsiRequesterActor extends Actor {
    def receive = LoggingReceive {
      case 'healthCheck =>
        sender ! Future.successful("NSI requester (Dummy)" -> true)
      case ToProvider(message @ NsiProviderMessage(headers, reserve: InitialReserve), _) =>
        val connectionId = newConnectionId
        val confirmCriteria = Conversion.invert(reserve.body.getCriteria()).get
        confirmCriteria.getPointToPointService.foreach { service =>
          service.setSourceSTP(qualifyStp(service.getSourceSTP))
          service.setDestSTP(qualifyStp(service.getDestSTP))
        }
        Connection(sender) ! Connection.Command(new Instant(), AckFromProvider(message ack ReserveResponse(connectionId)))
        Connection(sender) ! Connection.Command(new Instant(), FromProvider(message reply ReserveConfirmed(connectionId, confirmCriteria)))
      case ToProvider(message @ NsiProviderMessage(headers, reserve: ModifyReserve), _) =>
        Connection(sender) ! Connection.Command(new Instant(), AckFromProvider(message ack ReserveResponse(reserve.connectionId)))
        Connection(sender) ! Connection.Command(new Instant(), FromProvider(message reply ReserveConfirmed(reserve.connectionId, Conversion.invert(reserve.body.getCriteria()).get)))
      case ToProvider(message @ NsiProviderMessage(headers, commit: ReserveCommit), _) =>
        Connection(sender) ! Connection.Command(new Instant(), AckFromProvider(message ack GenericAck()))
        Connection(sender) ! Connection.Command(new Instant(), FromProvider(message reply ReserveCommitConfirmed(commit.connectionId)))
      case ToProvider(message @ NsiProviderMessage(headers, provision: Provision), _) =>
        Connection(sender) ! Connection.Command(new Instant(), AckFromProvider(message ack GenericAck()))
        Connection(sender) ! Connection.Command(new Instant(), FromProvider(message reply ProvisionConfirmed(provision.connectionId)))
      case ToProvider(message @ NsiProviderMessage(headers, terminate: Terminate), _) =>
        Connection(sender) ! Connection.Command(new Instant(), AckFromProvider(message ack GenericAck()))
        Connection(sender) ! Connection.Command(new Instant(), FromProvider(message reply TerminateConfirmed(terminate.connectionId)))
      case ToProvider(message @ NsiProviderMessage(headers, update: NsiProviderUpdateCommand), provider) =>
        Connection(sender) ! Connection.Command(new Instant(), AckFromProvider(message ack ServiceException(NsiError.NotImplemented.toServiceException(provider.nsa).withConnectionId(update.connectionId))))
      case ToProvider(message @ NsiProviderMessage(headers, query: QueryRecursive), provider) =>
        Connection(sender) ! Connection.Command(new Instant(), AckFromProvider(message ack ServiceException(NsiError.NotImplemented.toServiceException(provider.nsa))))
      case ToProvider(message @ NsiProviderMessage(headers, query: NsiProviderQuery), provider) =>
        Connection(sender) ! Connection.Command(new Instant(), AckFromProvider(message ack ServiceException(NsiError.NotImplemented.toServiceException(provider.nsa))))
    }

    private def qualifyStp(s: String): String = Stp.fromString(s).map { stp =>
      stp.vlan match {
        case None => stp.toString
        case Some(vlanRange) => stp.withLabel("vlan", vlanRange.lowerBound.toString).toString
      }
    }.getOrElse(s)
  }

}
