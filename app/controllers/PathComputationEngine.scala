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

object PathComputationEngine extends Controller {
  private val pceContinuations = new Continuations[PceResponse]()

  def pceReplyUrl: String = s"${Application.baseUrl}${routes.PathComputationEngine.pceReply().url}"

  def pceReply = Action(parse.json) { implicit request =>
    Json.fromJson[PceResponse](request.body) match {
      case JsSuccess(response, _) =>
        Logger.info(s"Pce reply: $response")
        pceContinuations.replyReceived(response.correlationId, response)
        Ok
      case JsError(error) =>
        Logger.info(s"Pce error: $error")
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
        pceContinuations.register(request.correlationId).onSuccess {
          case reply => connection ! FromPce(reply)
        }
        Logger.info(s"Sending request to pce ($endPoint): ${Json.toJson(request)}")
        WS.url(endPoint).post(Json.toJson(request))
    }
  }

  class DummyPceRequesterActor extends Actor {
    def receive = {
      case ToPce(pce: PathComputationRequest) =>
        val p2ps = pce.criteria.getP2Ps().getOrElse(throw new IllegalArgumentException(s"P2P service is missing: ${pce.criteria}"))
        sender !
          FromPce(PathComputationConfirmed(
            pce.correlationId,
            Seq(ComputedSegment(
              p2ps.getSourceSTP,
              p2ps.getDestSTP(),
              ProviderEndPoint(
                "urn:ogf:network:nsa:surfnet.nl",
                URI.create("http://localhost:8082/bod/nsi/v2/provider"),
                OAuthAuthentication("4e54d76e-8767-4e6d-95a8-e2b7387e53bb"))))))
    }
  }

}
