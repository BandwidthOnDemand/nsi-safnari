package controllers

import java.net.URI

import akka.actor._
import controllers.ActorSupport._
import nl.surfnet.nsiv2.messages.CorrelationId
import nl.surfnet.safnari._
import org.joda.time.{DateTime, Instant}
import play.api.Logger
import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json._
import play.api.libs.ws.WS
import play.api.mvc._

import scala.concurrent.Future
import scala.concurrent.stm.Ref
import scala.util.{Failure, Success}

object PathComputationEngine extends Controller {
  private val pceContinuations = new Continuations[PceResponse](actorSystem.scheduler)

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

  class PceRequesterActor(endPoint: String) extends Actor {
    private val uuidGenerator = Uuid.randomUuidGenerator()
    private def newCorrelationId() = CorrelationId.fromUuid(uuidGenerator())
    private val latestReachabilityEntries: LastModifiedCache[Seq[ReachabilityTopologyEntry]] = new LastModifiedCache()

    def receive = {
      case 'healthCheck =>
        val topologyHealth = WS.url(s"$endPoint/management/status/topology").withHeaders(ACCEPT -> JSON).get()

        topologyHealth onFailure { case e => Logger.warn(s"Failed to access PCE topology service: $e") }

        val lastModified = topologyHealth map { _.header("Last-Modified").getOrElse("unknown") }
        val healthy = topologyHealth.map(_.status == 200).recover { case t => false }

        sender ! healthy.flatMap(h => lastModified.map(d => s"PCE (Real; $d)" -> h))

      case 'reachability =>
        val reachabilityResponse = WS.url(s"$endPoint/reachability").withRequestTimeout(20000).withHeaders(ACCEPT -> JSON).get()
        val senderRef = sender

        reachabilityResponse.onComplete {
          case Success(response) =>
            val result = (response.json \ "reachability").validate[Seq[ReachabilityTopologyEntry]] match {
              case JsSuccess(reachability, _) =>
                Success(latestReachabilityEntries.updateAndGet(reachability))
              case JsError(e) =>
                Logger.error(s"Failed to parse reachability from the pce: $e")
                latestReachabilityEntries.get.toTry(new RuntimeException("Could not parse reachability"))
            }

            senderRef ! result
          case Failure(e) =>
            Logger.error("Failed to retrieve reachability from the pce", e)
            senderRef ! latestReachabilityEntries.get.toTry(e)
        }

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

    class LastModifiedCache[T] {
      private val value = Ref(None: Option[(T, DateTime)])

      def get: Option[(T, DateTime)] = value.single()

      def updateAndGet(newSubject: T): (T, DateTime) =
        value.single.transformAndGet {
          case Some(unchanged@(old, _)) if old == newSubject => Some(unchanged)
          case _ => Some(newSubject -> DateTime.now)
        }.get
    }
  }

  class DummyPceRequesterActor extends Actor {
    private val Reachability = (
      List(
        ReachabilityTopologyEntry("urn:ogf:network:surfnet.nl:1990:nsa:bod-dev", 0),
        ReachabilityTopologyEntry("urn:ogf:network:es.net:2013:nsa:oscars", 3)),
      DateTime.now())

    def receive = {
      case 'healthCheck =>
        sender ! Future.successful("PCE (Dummy)" -> true)

      case 'reachability =>
        sender ! Success(Reachability)

      case ToPce(pce: PathComputationRequest) =>
        Connection(sender) ! Connection.Command(new Instant(),
          FromPce(PathComputationConfirmed(
            pce.correlationId,
            Seq(ComputedSegment(
              ProviderEndPoint("urn:ogf:network:surfnet.nl:1990:nsa:bod-dev", URI.create("http://localhost:8082/bod/nsi/v2/provider")),
              pce.serviceType)))))
    }
  }
}
