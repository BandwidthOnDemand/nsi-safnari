package controllers

import akka.actor._
import java.net.URI
import nl.surfnet.safnari._
import play.api.Play.current
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json._
import play.api.libs.ws.WS
import play.api.mvc._
import play.api.Logger
import scala.concurrent.duration._

object PathComputationEngine extends Controller {
  private val pceContinuations = new Continuations[PceResponse](Akka.system.scheduler)

  def pceReplyUrl: String = s"${Configuration.BaseUrl}${routes.PathComputationEngine.pceReply().url}"

  def pceReply = Action(parse.json) { implicit request =>
    Json.fromJson[PceResponse](request.body) match {
      case JsSuccess(response, _) =>
        Logger.info(s"Pce reply: $response")
        pceContinuations.replyReceived(response.correlationId, response)
        Ok
      case JsError(error) =>
        Logger.info(s"Pce error: $error body: ${request.body}")
        BadRequest
    }
  }

  def pceRequester = {
    val pceEndpoint = current.configuration.getString("pce.endpoint").getOrElse(sys.error("pce.endpoint configuration property is not set"))
    current.configuration.getString("pce.actor") match {
      case None | Some("dummy") => Akka.system.actorOf(Props[DummyPceRequesterActor])
      case _                    => Akka.system.actorOf(Props(new PceRequesterActor(pceEndpoint)))
    }
  }

  class PceRequesterActor(endPoint: String) extends Actor {
    def receive = {
      case ToPce(request) =>
        val connection = sender
        pceContinuations.register(request.correlationId, Configuration.AsyncReplyTimeout).onSuccess {
          case reply => connection ! FromPce(reply)
        }
        Logger.info(s"Sending request to pce ($endPoint): ${Json.toJson(request)}")
        WS.url(endPoint).post(Json.toJson(request)).onFailure{
          case e => Logger.error(s"Could not reach the pce ($endPoint): $e")
        }
    }
  }

  class DummyPceRequesterActor extends Actor {
    def receive = {
      case ToPce(pce: PathComputationRequest) =>
        sender !
          FromPce(PathComputationConfirmed(
            pce.correlationId,
            Seq(ComputedSegment(
              ProviderEndPoint(
                "urn:ogf:network:surfnet.nl:1990:nsa:bod-dev",
                URI.create("http://localhost:8082/bod/nsi/v2/provider"),
                OAuthAuthentication("f44b1e47-0a19-4c11-861b-c9abf82d4cbf")),
              pce.serviceType))))
    }
  }
}
