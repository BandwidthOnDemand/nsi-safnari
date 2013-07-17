package controllers

import scala.concurrent.Future
import nl.surfnet.safnari._
import nl.surfnet.safnari.NsiSoapConversions._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc.Controller
import support.ExtraBodyParsers.NsiRequesterEndPoint
import akka.actor.Actor
import java.net.URI
import play.api.libs.ws.WS
import com.ning.http.client.Realm.AuthScheme
import support.ExtraBodyParsers._
import play.api.Play.current
import play.api.libs.concurrent.Akka
import akka.actor.Props
import play.api.Logger

object ConnectionRequester extends Controller with SoapWebService {

  val BaseWsdlFilename = "ogf_nsi_connection_requester_v2_0.wsdl"

  override def serviceUrl: String = s"${Application.baseUrl}${routes.ConnectionRequester.request().url}"

  def expectReplyFor(correlationId: CorrelationId): Future[NsiRequesterOperation] = continuations.register(correlationId)

  def request = NsiRequesterEndPoint {
    case notification: DataPlaneStateChange =>
      val connection = ConnectionProvider.connectionManager.findByChildConnectionId(notification.connectionId)
      connection foreach { _ ! FromProvider(notification) }
      val reply = connection map (_ => GenericAck(notification.headers.asReply)) getOrElse ServiceException(notification.headers.asReply, NsiError.DoesNotExist.toServiceException(Configuration.Nsa))
      Future.successful(reply)
    case response =>
      continuations.replyReceived(response.correlationId, response)
      Future.successful(GenericAck(response.headers.asReply))
  }

  def nsiRequester = {
    val requesterNsa = Configuration.Nsa
    current.configuration.getString("nsi.actor") match {
      case None | Some("dummy") => Akka.system.actorOf(Props[DummyNsiRequesterActor])
      case _                    => Akka.system.actorOf(Props(new NsiRequesterActor(requesterNsa, URI.create(ConnectionRequester.serviceUrl))))
    }
  }

  private val continuations = new Continuations[NsiRequesterOperation]()

  class DummyNsiRequesterActor extends Actor {
    def receive = {
      case ToProvider(reserve: Reserve, _) =>
        sender ! FromProvider(ReserveConfirmed(reserve.headers.asReply, newConnectionId, Conversion.invert(reserve.body.getCriteria()).right.get))
      case ToProvider(commit: ReserveCommit, _) =>
        sender ! FromProvider(ReserveCommitConfirmed(commit.headers.asReply, commit.connectionId))
    }
  }

  class NsiRequesterActor(requesterNsa: String, requesterUrl: URI) extends Actor {
    def receive = {
      case ToProvider(message: NsiProviderOperation, provider) =>
        val connection = sender
        ConnectionRequester.expectReplyFor(message.correlationId).onSuccess {
          case reply => connection ! FromProvider(reply)
        }

        var request = WS.url(provider.url.toASCIIString())

        request = provider.authentication match {
          case OAuthAuthentication(token)              => request.withHeaders("Authorization" -> s"bearer $token")
          case BasicAuthentication(username, password) => request.withAuth(username, password, AuthScheme.BASIC)
          case _                                       => request
        }

        request.post(message).onFailure {
          case e => Logger.warn(s"Could not reach nsi provider $e")
        }
    }
  }

}
