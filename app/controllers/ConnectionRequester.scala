package controllers

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import java.net.URI
import java.util.concurrent.TimeoutException
import nl.surfnet.safnari._
import org.joda.time.DateTime
import org.ogf.schemas.nsi._2013._07.connection.types.MessageDeliveryTimeoutRequestType
import play.api.Logger
import play.api.Play.current
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc.Controller
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{ Failure, Success }
import support.ExtraBodyParsers._
import org.ogf.schemas.nsi._2013._07.connection.types.QueryRecursiveResultType

class ConnectionRequester(connectionManager: ConnectionManager) extends Controller with SoapWebService {
  implicit val timeout = Timeout(2.seconds)

  val BaseWsdlFilename = "ogf_nsi_connection_requester_v2_0.wsdl"

  override def serviceUrl: String = ConnectionRequester.serviceUrl

  def request = NsiRequesterEndPoint(Configuration.Nsa) {
    case message @ NsiRequesterMessage(headers, notification: NsiNotification) =>
      val connection = connectionManager.findByChildConnectionId(notification.connectionId)

      val ack = connection.map { c =>
        (c ? FromProvider(NsiRequesterMessage(headers, notification))).mapTo[NsiAcknowledgement]
      } getOrElse Future.successful(ServiceException(NsiError.ConnectionNonExistent.toServiceException(Configuration.Nsa)))

      ack.map(message.ack)
    case response =>
      ConnectionRequester.continuations.replyReceived(response.headers.correlationId, response)
      // FIXME return error when message cannot be handled?
      Future.successful(response.ack(GenericAck()))
  }

}

object ConnectionRequester {
  implicit def actorSystem = Akka.system

  def serviceUrl: String = s"${Configuration.BaseUrl}${routes.ConnectionRequester.request().url}"
  val continuations = new Continuations[NsiRequesterMessage[NsiRequesterOperation]](actorSystem.scheduler)

  def nsiRequester: ActorRef = {
    val requesterNsa = Configuration.Nsa
    current.configuration.getString("nsi.actor") match {
      case None | Some("dummy") => Akka.system.actorOf(Props[DummyNsiRequesterActor])
      case _                    => Akka.system.actorOf(Props(new NsiRequesterActor(requesterNsa, URI.create(serviceUrl))))
    }
  }

  class NsiRequesterActor(requesterNsa: String, requesterUrl: URI) extends Actor {
    def receive = {
      case ToProvider(message @ NsiProviderMessage(headers, operation: NsiProviderOperation), provider) =>
        val connectionId = operation match {
          case command: NsiProviderCommand => command.optionalConnectionId
          case _ => None
        }

        val connection = sender
        continuations.register(headers.correlationId, Configuration.AsyncReplyTimeout).onComplete {
          case Success(reply) =>
            connection ! FromProvider(reply)
          case Failure(exception) =>
            // FIXME maybe handle timeouts separately?
            connection ! MessageDeliveryFailure(headers.correlationId, connectionId, provider.url, DateTime.now(), exception.toString)
        }

        val response = NsiWebService.callProvider(provider, message)
        response.onComplete {
          case Failure(timeout: TimeoutException) =>
            // Let the requester timeout as well (or receive an actual reply). No need to send an ack timeout!
          case Failure(exception) =>
            Logger.warn(s"communication failure calling $provider", exception)
            continuations.unregister(headers.correlationId)
            connection ! MessageDeliveryFailure(headers.correlationId, connectionId, provider.url, DateTime.now(), exception.toString)
          case Success(ack @ NsiProviderMessage(_, ServiceException(_))) =>
            continuations.unregister(headers.correlationId)
            connection ! AckFromProvider(ack)
          case Success(ack) =>
            connection ! AckFromProvider(ack)
        }
    }
  }

  class DummyNsiRequesterActor extends Actor {
    def receive = {
      case ToProvider(message @ NsiProviderMessage(headers, reserve: InitialReserve), _) =>
        val connectionId = newConnectionId
        sender ! AckFromProvider(message ack ReserveResponse(connectionId))
        sender ! FromProvider(message reply ReserveConfirmed(connectionId, Conversion.invert(reserve.body.getCriteria()).right.get))
      case ToProvider(message @ NsiProviderMessage(headers, commit: ReserveCommit), _) =>
        sender ! AckFromProvider(message ack GenericAck())
        sender ! FromProvider(message reply ReserveCommitConfirmed(commit.connectionId))
      case ToProvider(message @ NsiProviderMessage(headers, provision: Provision), _) =>
        sender ! AckFromProvider(message ack GenericAck())
        sender ! FromProvider(message reply ProvisionConfirmed(provision.connectionId))
      case ToProvider(message @ NsiProviderMessage(headers, update: NsiProviderUpdateCommand), provider) =>
        sender ! AckFromProvider(message ack ServiceException(NsiError.NotImplemented.toServiceException(provider.nsa).withConnectionId(update.connectionId)))
      case ToProvider(message @ NsiProviderMessage(headers, query: QueryRecursive), provider) =>
        sender ! AckFromProvider(message ack ServiceException(NsiError.NotImplemented.toServiceException(provider.nsa)))
      case ToProvider(message @ NsiProviderMessage(headers, query: NsiProviderQuery), provider) =>
        sender ! AckFromProvider(message ack ServiceException(NsiError.NotImplemented.toServiceException(provider.nsa)))
    }
  }

}
