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
import scala.util.{ Success, Failure }
import scala.concurrent.duration._
import scala.concurrent.Future
import org.joda.time.DateTime
import java.util.concurrent.TimeoutException

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

  def pceRequester: ActorRef = {
    val pceEndpoint = current.configuration.getString("pce.endpoint").getOrElse(sys.error("pce.endpoint configuration property is not set"))
    current.configuration.getString("pce.actor") match {
      case None | Some("dummy") => Akka.system.actorOf(Props[DummyPceRequesterActor])
      case _                    => Akka.system.actorOf(Props(new PceRequesterActor(pceEndpoint)))
    }
  }

  class PceRequesterActor(endPoint: String) extends Actor {
    private val uuidGenerator = Uuid.randomUuidGenerator()
    private def newCorrelationId() = CorrelationId.fromUuid(uuidGenerator())

    def receive = {
      case 'healthCheck =>
        val topologyHealth = WS.url(s"$endPoint/topology/ping").get()
        val pathsHealth = WS.url(s"$endPoint/paths").get()

        topologyHealth onFailure { case e => Logger.warn(s"Failed to access PCE topology service: $e") }
        pathsHealth onFailure { case e => Logger.warn(s"Failed to access PCE path finding service: $e") }

        val lastModified = topologyHealth map { _.header("Last-Modified").getOrElse("unknown") }
        val healthy = Future.sequence(List(topologyHealth, pathsHealth)).map(_.forall(_.status == 200)).recover { case t => false }

        sender ! healthy.flatMap(h => lastModified.map(d => s"PCE (Real; $d)" -> h))

      case ToPce(request) =>
        val findPathEndPoint = s"$endPoint/paths/find"
        Logger.info(s"Sending request to pce ($findPathEndPoint): ${Json.toJson(request)}")

        val connection = sender
        pceContinuations.register(request.correlationId, Configuration.AsyncReplyTimeout).onComplete {
          case Success(reply) =>
            connection ! FromPce(reply)
          case Failure(e) =>
            connection ! MessageDeliveryFailure(newCorrelationId(), None, request.correlationId, URI.create(findPathEndPoint), DateTime.now(), e.toString)
        }

        val response = WS.url(findPathEndPoint).post(Json.toJson(request))
        response onComplete {
          case Failure(e) =>
            Logger.error(s"Could not reach the pce ($endPoint): $e")
            pceContinuations.unregister(request.correlationId)
            connection ! MessageDeliveryFailure(newCorrelationId(), None, request.correlationId, URI.create(findPathEndPoint), DateTime.now(), e.toString)
          case Success(response) if response.status == ACCEPTED =>
            connection ! AckFromPce(PceAccepted(request.correlationId))
          case Success(response) =>
            Logger.error(s"Got back a ${response.status} response from the PCE: ${response.body}")
            pceContinuations.unregister(request.correlationId)
            connection ! AckFromPce(PceFailed(request.correlationId, response.status, response.statusText, response.body))
        }

    }
  }

  class DummyPceRequesterActor extends Actor {
    def receive = {
      case 'healthCheck =>
        sender ! Future.successful("PCE (Dummy)" -> true)

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
