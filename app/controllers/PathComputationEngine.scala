/*
 * Copyright (c) 2012, 2013, 2014, 2015, 2016 SURFnet BV
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the
 * following conditions are met:
 *
 *   * Redistributions of source code must retain the above copyright notice, this list of conditions and the following
 *     disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following
 *     disclaimer in the documentation and/or other materials provided with the distribution.
 *   * Neither the name of the SURFnet BV nor the names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package controllers

import java.net.URI
import javax.inject._

import akka.actor._
import nl.surfnet.nsiv2.messages.CorrelationId
import nl.surfnet.nsiv2.utils._
import nl.surfnet.safnari._
import java.time.Instant
import play.api.Logger
import play.api.http.ContentTypes._
import play.api.http.HeaderNames._
import play.api.http.Status._
import play.api.libs.json._
import play.api.libs.ws.WSClient
import play.api.mvc._

import scala.concurrent.{ ExecutionContext, Future }
import scala.concurrent.duration._
import scala.concurrent.stm.Ref
import scala.util.{Failure, Success}

@Singleton
class PathComputationEngineController @Inject()(pce: PathComputationEngine) extends InjectedController {
  private val logger = Logger(classOf[PathComputationEngineController])

  def pceReply = Action(parse.json) { implicit request =>
    Json.fromJson[PceResponse](request.body) match {
      case JsSuccess(response, _) =>
        logger.info(s"Pce reply: $response")
        pce.pceContinuations.replyReceived(response.correlationId, response)
        Ok
      case JsError(error) =>
        logger.info(s"Pce error: $error body: ${request.body}")
        BadRequest
    }
  }
}

@Singleton
class PathComputationEngine @Inject()(actorSystem: ActorSystem, ws: WSClient)(implicit ec: ExecutionContext) {
  private val logger = Logger(classOf[PathComputationEngine])

  private[controllers] val pceContinuations = new Continuations[PceResponse](actorSystem.scheduler)

  def createPceRequesterActor(configuration: Configuration): ActorRef =
    configuration.PceActor match {
      case None | Some("dummy") => actorSystem.actorOf(Props(new DummyPceRequesterActor), "pceRequester")
      case _                    => actorSystem.actorOf(Props(new PceRequesterActor(configuration)), "pceRequester")
    }

  class PceRequesterActor(configuration: Configuration) extends Actor {
    private val uuidGenerator = Uuid.randomUuidGenerator()
    private def newCorrelationId() = CorrelationId.fromUuid(uuidGenerator())
    private val latestReachabilityEntries: LastModifiedCache[Seq[ReachabilityTopologyEntry]] = new LastModifiedCache()
    private val endPoint = configuration.PceEndpoint

    def receive = {
      case HealthCheck =>
        val topologyHealth = ws.url(s"$endPoint/management/status/topology").addHttpHeaders(ACCEPT -> JSON).get()

        topologyHealth onComplete {
          case Success(_) => // nothing
          case Failure(e) => logger.warn(s"Failed to access PCE topology service: $e")
        }

        val lastModified = topologyHealth map { _.header("Last-Modified").getOrElse("unknown") }
        val healthy = topologyHealth.map(_.status == 200).recover { case _ => false }

        sender() ! healthy.flatMap(h => lastModified.map(d => s"PCE (Real; $d)" -> h))

      case ReachabilityCheck =>
        val reachabilityResponse = ws.url(s"$endPoint/reachability").withRequestTimeout(Duration(20000, MILLISECONDS)).addHttpHeaders(ACCEPT -> JSON).get()
        val senderRef = sender()

        reachabilityResponse.onComplete {
          case Success(response) =>
            val result = (response.json \ "reachability").validate[Seq[ReachabilityTopologyEntry]] match {
              case JsSuccess(reachability, _) =>
                Success(latestReachabilityEntries.updateAndGet(reachability))
              case JsError(e) =>
                logger.error(s"Failed to parse reachability from the pce: $e")
                latestReachabilityEntries.get.toTry(new RuntimeException("Could not parse reachability"))
            }

            senderRef ! result
          case Failure(e) =>
            logger.error("Failed to retrieve reachability from the pce", e)
            senderRef ! latestReachabilityEntries.get.toTry(e)
        }

      case ToPce(request) =>
        val findPathEndPoint = s"$endPoint/paths/find"
        logger.info(s"Sending request to pce ($findPathEndPoint): ${Json.toJson(request)}")

        val connection = Connection(sender())
        pceContinuations.register(request.correlationId, configuration.AsyncReplyTimeout).onComplete {
          case Success(reply) =>
            connection ! Connection.Command(Instant.now, FromPce(reply))
          case Failure(e) =>
            connection ! Connection.Command(Instant.now, MessageDeliveryFailure(newCorrelationId(), None, request.correlationId, URI.create(findPathEndPoint), Instant.now(), e.toString))
        }

        val response = ws.url(findPathEndPoint).post(Json.toJson(request))
        response onComplete {
          case Failure(e) =>
            logger.error(s"Could not reach the pce ($endPoint): $e")
            pceContinuations.unregister(request.correlationId)
            connection ! Connection.Command(Instant.now, MessageDeliveryFailure(newCorrelationId(), None, request.correlationId, URI.create(findPathEndPoint), Instant.now(), e.toString))
          case Success(response) if response.status == ACCEPTED =>
            connection ! Connection.Command(Instant.now, AckFromPce(PceAccepted(request.correlationId)))
          case Success(response) =>
            logger.error(s"Got back a ${response.status} response from the PCE: ${response.body}")
            pceContinuations.unregister(request.correlationId)
            connection ! Connection.Command(Instant.now, AckFromPce(PceFailed(request.correlationId, response.status, response.statusText, response.body)))
        }
    }

    class LastModifiedCache[T] {
      private val value = Ref(None: Option[(T, Instant)])

      def get: Option[(T, Instant)] = value.single()

      def updateAndGet(newSubject: T): (T, Instant) =
        value.single.transformAndGet {
          case Some(unchanged@(old, _)) if old == newSubject => Some(unchanged)
          case _ => Some(newSubject -> Instant.now)
        }.get
    }
  }

  class DummyPceRequesterActor extends Actor {
    private val Reachability = (
      List(
        ReachabilityTopologyEntry("urn:ogf:network:surfnet.nl:1990:nsa:bod-dev", 0),
        ReachabilityTopologyEntry("urn:ogf:network:es.net:2013:nsa:oscars", 3)),
      Instant.now())

    def receive = {
      case HealthCheck =>
        sender() ! Future.successful("PCE (Dummy)" -> true)

      case ReachabilityCheck =>
        sender() ! Success(Reachability)

      case ToPce(pce: PathComputationRequest) =>
        val serviceType = Json.fromJson[ServiceType](Json.toJson(pce.serviceType)(PceMessage.ServiceTypeFormat))(PceMessage.ServiceTypeFormat).get

        Connection(sender()) ! Connection.Command(Instant.now,
          FromPce(PathComputationConfirmed(
            pce.correlationId,
            Seq(ComputedSegment(
              ProviderEndPoint("urn:ogf:network:surfnet.nl:1990:nsa:bod-dev", URI.create("http://localhost:8082/bod/nsi/v2/provider")),
              serviceType)))))
    }
  }
}
