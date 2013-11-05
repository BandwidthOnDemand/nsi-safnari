package controllers

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import com.ning.http.client.Realm.AuthScheme
import java.net.URI
import nl.surfnet.safnari._
import nl.surfnet.safnari.NsiSoapConversions._
import play.api.Logger
import play.api.Play.current
import play.api.http.Status._
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc.Controller
import scala.concurrent.Future
import scala.concurrent.duration._
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

        val response = NsiWebService.callProvider(provider, message)
        response.onComplete {
          case Failure(error) => Logger.error(s"error calling $provider: $error", error)
          case Success(ack)   => connection ! AckFromProvider(ack)
        }
    }
  }
}
