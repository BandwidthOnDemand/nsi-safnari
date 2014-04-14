package controllers

import akka.actor._
import java.net.URI
import java.util.concurrent.TimeoutException
import nl.surfnet.safnari._
import org.joda.time.DateTime
import org.joda.time.Instant
import play.api.Logger
import play.api.Play.current
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json._
import play.api.libs.ws.WS
import play.api.mvc._
import play.api.http.HeaderNames.ACCEPT
import play.api.http.MimeTypes.JSON
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{ Success, Failure }
import nl.surfnet.safnari.ReachabilityTopologyEntry

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
        val topologyHealth = WS.url(s"$endPoint/management/status/topology").withHeaders(ACCEPT -> JSON).get()

        topologyHealth onFailure { case e => Logger.warn(s"Failed to access PCE topology service: $e") }

        val lastModified = topologyHealth map { _.header("Last-Modified").getOrElse("unknown") }
        val healthy = topologyHealth.map(_.status == 200).recover { case t => false }

        sender ! healthy.flatMap(h => lastModified.map(d => s"PCE (Real; $d)" -> h))

      case 'reachability =>
        val reachabilityResponse = WS.url(s"$endPoint/reachability").withHeaders(ACCEPT -> JSON).get()

        val reachability = reachabilityResponse.map { response =>
          implicit val reachabilityTopologyEntryReader = Json.reads[ReachabilityTopologyEntry]

          (response.json \ "reachability").validate[Seq[ReachabilityTopologyEntry]].fold(error => { Logger.error(""); Nil }, identity)
        }

        sender ! reachability

      case ToPce(request) =>
        val findPathEndPoint = s"$endPoint/paths/find"
        Logger.info(s"Sending request to pce ($findPathEndPoint): ${Json.toJson(request)}")

        val connection = Connection(sender)
        pceContinuations.register(request.correlationId, Configuration.AsyncReplyTimeout).onComplete {
          case Success(reply) =>
            connection ! Connection.Command(new Instant(), FromPce(reply))
          case Failure(e) =>
            connection ! Connection.Command(new Instant(), MessageDeliveryFailure(newCorrelationId(), None, request.correlationId, URI.create(findPathEndPoint), DateTime.now(), e.toString))
        }

        val response = WS.url(findPathEndPoint).post(Json.toJson(request))
        response onComplete {
          case Failure(e) =>
            Logger.error(s"Could not reach the pce ($endPoint): $e")
            pceContinuations.unregister(request.correlationId)
            connection ! Connection.Command(new Instant(), MessageDeliveryFailure(newCorrelationId(), None, request.correlationId, URI.create(findPathEndPoint), DateTime.now(), e.toString))
          case Success(response) if response.status == ACCEPTED =>
            connection ! Connection.Command(new Instant(), AckFromPce(PceAccepted(request.correlationId)))
          case Success(response) =>
            Logger.error(s"Got back a ${response.status} response from the PCE: ${response.body}")
            pceContinuations.unregister(request.correlationId)
            connection ! Connection.Command(new Instant(), AckFromPce(PceFailed(request.correlationId, response.status, response.statusText, response.body)))
        }
    }
  }

  class DummyPceRequesterActor extends Actor {
    def receive = {
      case 'healthCheck =>
        sender ! Future.successful("PCE (Dummy)" -> true)

      case 'reachability =>
        val reachability =
          ReachabilityTopologyEntry("urn:ogf:network:surfnet.nl:1990:nsa:bod-dev", 0) ::
          ReachabilityTopologyEntry("urn:ogf:network:es.net:2013:nsa:oscars", 3) ::
          Nil

        sender ! Future.successful(reachability)

      case ToPce(pce: PathComputationRequest) =>
        Connection(sender) ! Connection.Command(new Instant(),
          FromPce(PathComputationConfirmed(
            pce.correlationId,
            Seq(ComputedSegment(
              ProviderEndPoint(
                "urn:ogf:network:surfnet.nl:1990:nsa:bod-dev",
                URI.create("http://localhost:8082/bod/nsi/v2/provider"),
                OAuthAuthentication("f44b1e47-0a19-4c11-861b-c9abf82d4cbf")),
              pce.serviceType)))))
    }
  }
}
