package controllers

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import com.ning.http.client.Realm.AuthScheme
import controllers.SoapRequests._
import java.net.URI
import nl.surfnet.safnari._
import nl.surfnet.safnari.NsiSoapConversions._
import play.api.Logger
import play.api.Play.current
import play.api.http.Status._
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.ws.WS
import play.api.mvc.Controller
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util.{ Failure, Success }
import support.ExtraBodyParsers._

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
            connection ! ErrorFromProvider(headers, None, NsiError.ChildError.toServiceException(provider.nsa))
          case Success(ack) =>
            ack.status match {
              case OK | CREATED | ACCEPTED | INTERNAL_SERVER_ERROR =>
                Logger.debug(s"Parsing SOAP ack (${ack.status}) from ${provider.nsa} at ${provider.url}: ${ack.body}")
                Conversion[NsiProviderMessage[NsiAcknowledgement], String].invert(ack.body) match {
                  case Left(error) =>
                    Logger.warn(s"Communication error with provider ${provider.nsa} at ${provider.url}: $error")
                    connection ! ErrorFromProvider(headers, None, NsiError.ChildError.copy(text = error).toServiceException(provider.nsa))
                  case Right(ack @ NsiProviderMessage(ackHeaders, ServiceException(exception))) =>
                    Logger.debug(s"Received failed ack from ${provider.nsa} at ${provider.url}: $ack")
                    connection ! ErrorFromProvider(headers, Some(ackHeaders), exception)
                  case Right(ack) =>
                    Logger.debug(s"Received ack from ${provider.nsa} at ${provider.url}: $ack")
                    connection ! AckFromProvider(ack)
                }
              case FORBIDDEN =>
                Logger.warn(s"Authentication failed (${ack.status}) from ${provider.nsa} at ${provider.url}: ${ack.body}")
                connection ! ErrorFromProvider(headers, None, NsiError.AuthenticationFailure.toServiceException(provider.nsa))
              case _ =>
                Logger.warn(s"Communication error with provider ${provider.nsa} at ${provider.url}: ${ack.status} ${ack.statusText} ${ack.header("content-type")}\n\t${ack.body}")
                connection ! ErrorFromProvider(headers, None, NsiError.ChildError.copy(text = s"Communication error: ${ack.status} ${ack.statusText}").toServiceException(provider.nsa))
            }
        }
    }
  }
}
