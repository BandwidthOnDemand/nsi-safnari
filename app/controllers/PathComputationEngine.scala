package controllers

import akka.actor._
import java.net.URI
import nl.surfnet.safnari._
import org.ogf.schemas.nsi._2013._04.connection.types.StpType
import play.api.Play.current
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json._
import play.api.libs.ws.WS
import play.api.mvc._

object PathComputationEngine extends Controller {
  private val pceContinuations = new Continuations[PceResponse]()

  def pceReplyUrl: String = s"${Application.baseUrl}${routes.PathComputationEngine.pceReply().url}"

  def pceReply = Action(parse.json) { implicit request =>
    Json.fromJson[PceResponse](request.body) match {
      case JsSuccess(response, _) =>
        pceContinuations.replyReceived(response.correlationId, response)
        Ok
      case JsError(error) =>
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
        WS.url(endPoint).post(Json.toJson(request))
    }
  }

  class DummyPceRequesterActor extends Actor {
    def receive = {
      case ToPce(pce: PathComputationRequest) =>
        sender !
          FromPce(PathComputationConfirmed(
            pce.correlationId,
            Seq(ComputedSegment(
              pce.criteria.getPath().getSourceSTP,
              pce.criteria.getPath().getDestSTP(),
              ProviderEndPoint(
                "urn:ogf:network:nsa:surfnet.nl",
                URI.create("http://localhost:8082/bod/nsi/v2/provider"),
                OAuthAuthentication("4e54d76e-8767-4e6d-95a8-e2b7387e53bb"))))))
    }
  }

}
