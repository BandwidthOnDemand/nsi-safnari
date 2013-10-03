package controllers

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import nl.surfnet.safnari._
import nl.surfnet.safnari.NsiSoapConversions._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc.Controller
import support.ExtraBodyParsers.NsiRequesterEndPoint
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import java.net.URI
import play.api.libs.ws.WS
import com.ning.http.client.Realm.AuthScheme
import support.ExtraBodyParsers._
import play.api.Play.current
import play.api.libs.concurrent.Akka
import akka.actor.Props
import play.api.Logger
import scala.util.{ Failure, Success }
import SoapRequests._

object ConnectionRequester extends Controller with SoapWebService {
  implicit val timeout = Timeout(2.seconds)
  implicit def actorSystem = Akka.system

  val BaseWsdlFilename = "ogf_nsi_connection_requester_v2_0.wsdl"

  override def serviceUrl: String = s"${Application.baseUrl}${routes.ConnectionRequester.request().url}"

  def expectReplyFor(correlationId: CorrelationId): Future[NsiRequesterMessage[NsiRequesterOperation]] = continuations.register(correlationId)

  def request = NsiRequesterEndPoint {
    case message @ NsiRequesterMessage(headers, notification: NsiNotification) =>
      val connection = ConnectionProvider.connectionManager.findByChildConnectionId(notification.connectionId)

      val ack = connection.map { c =>
        (c ? FromProvider(NsiRequesterMessage(headers, notification))).mapTo[NsiAcknowledgement]
      } getOrElse Future.successful(ServiceException(NsiError.ConnectionNonExistent.toServiceException(Configuration.Nsa)))

      ack.map(message.ack)
    case response =>
      continuations.replyReceived(response.headers.correlationId, response)
      Future.successful(response.ack(GenericAck()))
  }

  def nsiRequester = {
    val requesterNsa = Configuration.Nsa
    current.configuration.getString("nsi.actor") match {
      case None | Some("dummy") => Akka.system.actorOf(Props[DummyNsiRequesterActor])
      case _                    => Akka.system.actorOf(Props(new NsiRequesterActor(requesterNsa, URI.create(ConnectionRequester.serviceUrl))))
    }
  }

  private val continuations = new Continuations[NsiRequesterMessage[NsiRequesterOperation]]()

  class DummyNsiRequesterActor extends Actor {
    def receive = {
      case ToProvider(message @ NsiProviderMessage(headers, reserve: InitialReserve), _) =>
        sender ! FromProvider(message reply ReserveConfirmed(newConnectionId, Conversion.invert(reserve.body.getCriteria()).right.get))
      case ToProvider(message @ NsiProviderMessage(headers, commit: ReserveCommit), _) =>
        sender ! FromProvider(message reply ReserveCommitConfirmed(commit.connectionId))
    }
  }

  class NsiRequesterActor(requesterNsa: String, requesterUrl: URI) extends Actor {
    def receive = {
      case ToProvider(message @ NsiProviderMessage(headers, operation: NsiProviderOperation), provider) =>
        val connection = sender
        ConnectionRequester.expectReplyFor(headers.correlationId).onSuccess {
          case reply => connection ! FromProvider(reply)
        }

        var request = WS.url(provider.url.toASCIIString())

        request = provider.authentication match {
          case OAuthAuthentication(token)              => request.withHeaders("Authorization" -> s"bearer $token")
          case BasicAuthentication(username, password) => request.withAuth(username, password, AuthScheme.BASIC)
          case _                                       => request
        }

        request = request.withSoapActionHeader(operation.soapActionUrl)

        Logger.debug(s"Sending provider ${provider.nsa} at ${request.url} the SOAP message: ${Conversion[NsiProviderMessage[NsiProviderOperation], String].apply(message)}")

        request.post(message).onComplete {
          case Failure(e) =>
            Logger.warn(s"Communication error with provider ${provider.nsa} at ${provider.url}: $e", e)
          case Success(ack) if ack.status == 200 || ack.status == 500 =>
            Logger.debug(s"Parsing SOAP ack (${ack.status}) from ${provider.nsa} at ${provider.url}: ${ack.body}")
            Conversion[NsiProviderMessage[NsiAcknowledgement], String].invert(ack.body) match {
              case Left(error) =>
                Logger.warn(s"Communication error with provider ${provider.nsa} at ${provider.url}: $error")
              case Right(ack) =>
                Logger.debug(s"Received ack from ${provider.nsa} at ${provider.url}: $ack")
                connection ! AckFromProvider(ack)
            }
          case Success(ack) =>
            Logger.warn(s"Communication error with provider ${provider.nsa} at ${provider.url}: ${ack.status} ${ack.statusText} ${ack.header("content-type")}\n\t${ack.body}")
        }
    }
  }
}
