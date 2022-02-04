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
package nl.surfnet.safnari

import java.net.URI
import java.time.Instant
import javax.xml.XMLConstants
import javax.xml.namespace.QName
import javax.xml.datatype.{ DatatypeFactory, XMLGregorianCalendar }
import scala.collection.JavaConverters._
import nl.surfnet.nsiv2.messages._
import nl.surfnet.nsiv2.utils._

import net.nordu.namespaces._2013._12.gnsbod.ConnectionType
import org.ogf.schemas.nsi._2013._12.services.point2point.P2PServiceBaseType
import org.ogf.schemas.nsi._2013._12.services.types._
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.util.Try

case class ServiceType(serviceType: String, service: P2PServiceBaseType)

sealed trait PceMessage {
  final def action = this.getClass.getSimpleName
  def correlationId: CorrelationId
}
sealed trait PceRequest extends PceMessage
case class PathComputationRequest(
  correlationId: CorrelationId,
  nsaId: Option[String],
  replyTo: URI,
  startTime: Option[Instant],
  endTime: Option[Instant],
  serviceType: ServiceType,
  algorithm: PathComputationAlgorithm,
  connectionTrace: List[ConnectionType]
) extends PceRequest

sealed trait PathComputationAlgorithm { def name: String }
object PathComputationAlgorithm {
  case object Chain extends PathComputationAlgorithm { val name = "CHAIN" }
  case object Tree extends PathComputationAlgorithm { val name = "TREE" }
  case object Sequential extends PathComputationAlgorithm { val name = "SEQUENTIAL" }

  val values: Seq[PathComputationAlgorithm] = Vector(Chain, Tree, Sequential)
  def parse(value: String): Option[PathComputationAlgorithm] = values.find(_.name == value.toUpperCase())
}

sealed trait PceResponse extends PceMessage
case class PathComputationFailed(correlationId: CorrelationId, error: NsiError) extends PceResponse
case class PathComputationConfirmed(correlationId: CorrelationId, segments: Seq[ComputedSegment]) extends PceResponse

sealed trait PceAcknowledgement extends PceMessage
case class PceAccepted(correlationId: CorrelationId) extends PceAcknowledgement
case class PceFailed(correlationId: CorrelationId, status: Int, statusText: String, message: String) extends PceAcknowledgement

case class ProviderEndPoint(nsa: String, url: URI)

case class ComputedSegment(provider: ProviderEndPoint, serviceType: ServiceType)

case class ReachabilityTopologyEntry(id: String, cost: Int)

object ReachabilityTopologyEntry {
  implicit val ReachabilityTopologyEntryFormat: Format[ReachabilityTopologyEntry] = Json.format[ReachabilityTopologyEntry]
}

object PceMessage {
  implicit val CorrelationIdReads: Reads[CorrelationId] = Reads[CorrelationId] {
    case JsString(s) => CorrelationId.fromString(s).map { x => JsSuccess(x) }.getOrElse { JsError(JsonValidationError("bad.correlation.id", s)) }
    case json        => JsError(JsonValidationError("bad.correlation.id", json))
  }
  implicit val CorrelationIdWrites: Writes[CorrelationId] = Writes { x => JsString(x.toString) }

  implicit val PathFindingAlgorithmWrites: Writes[PathComputationAlgorithm] = Writes {
    case algo => JsString(algo.name)
  }
  implicit val PathFindingAlgorithmReads: Reads[PathComputationAlgorithm] = Reads { json =>
    json match {
      case JsString(value) => PathComputationAlgorithm.parse(value).fold[JsResult[PathComputationAlgorithm]](JsError(s"Unknown path computation algorithm '$value'"))(JsSuccess(_))
      case _ => JsError("Could not read path finding algorithm")
    }
  }

  implicit val OrderedStpTypeFormat: OFormat[OrderedStpType] = (
    (__ \ "@order").format[Int] and
      (__ \ "stp").format[String]
    ).apply((order, stp) => new OrderedStpType().withOrder(order).withStp(stp), orderedStpType => (orderedStpType.getOrder(), orderedStpType.getStp()))

  implicit val TypeValueTypeFormat: OFormat[TypeValueType] = (
    (__ \ "@type").format[String] and
      (__ \ "value").format[String]
    ).apply((tvType, value) => new TypeValueType().withType(tvType).withValue(value), typeValueType => (typeValueType.getType(), typeValueType.getValue()))

  implicit val StpListTypeFormat: OFormat[StpListType] =
    (__ \ "orderedSTP").format[Seq[OrderedStpType]].inmap((orderedSTP) => new StpListType().withOrderedSTP(orderedSTP.asJava), stpListType => (stpListType.getOrderedSTP().asScala.toSeq))

  implicit val ProviderEndPointFormat: OFormat[ProviderEndPoint] = (
    (__ \ "nsa").format[String] and
    (__ \ "csProviderURL").format[URI])(ProviderEndPoint.apply, unlift(ProviderEndPoint.unapply))

  implicit val pointToPointServiceFormat: OFormat[P2PServiceBaseType] = (
    (__ \ "capacity").format[Long] and
    (__ \ "directionality").formatNullable[String].inmap[Option[DirectionalityType]](_.map(DirectionalityType.fromValue(_)), _.map(_.value)) and
    (__ \ "symmetricPath").formatNullable[Boolean] and
    (__ \ "sourceSTP").format[String] and
    (__ \ "destSTP").format[String] and
    (__ \ "ero").formatNullable[StpListType] and
    (__ \ "parameter").format[Seq[TypeValueType]]
  ).apply(
    (capacity, directionality, symmetricPath, source, dest, ero, parameter) => {
      new P2PServiceBaseType()
        .withCapacity(capacity)
        .withDirectionality(directionality.getOrElse(DirectionalityType.BIDIRECTIONAL))
        .withSymmetricPath(symmetricPath.map(x => x: java.lang.Boolean).orNull)
        .withSourceSTP(source)
        .withDestSTP(dest)
        .withEro(ero.orNull)
        .withParameter(parameter.asJava)
    },
    {
      p2ps => (p2ps.getCapacity(), Option(p2ps.getDirectionality()), Option(p2ps.isSymmetricPath()).map(_.booleanValue()),
        p2ps.getSourceSTP(), p2ps.getDestSTP(), Option(p2ps.getEro()), p2ps.getParameter().asScala.toSeq)
    }
  )

  implicit val ServiceTypeFormat: OFormat[ServiceType] = (
    (__ \ "serviceType").format[String] and
    (__ \ "p.p2ps").format[Seq[P2PServiceBaseType]]
  ).apply(
    (st, p2ps) => ServiceType(st, p2ps.head),
    serviceType => (serviceType.serviceType, serviceType.service :: Nil)
  )

  implicit val ComputedSegmentFormat: Format[ComputedSegment] = (ProviderEndPointFormat and ServiceTypeFormat)(ComputedSegment.apply, unlift(ComputedSegment.unapply))

  implicit val PceResponseReads: Reads[PceResponse] = Reads { json =>
    (__ \ "status").read[String].reads(json) match {
      case JsSuccess("SUCCESS", _) => Json.fromJson[PathComputationConfirmed](json)
      case JsSuccess("FAILED", _)  => Json.fromJson[PathComputationFailed](json)
      case JsSuccess(status, path) => JsError(path -> JsonValidationError("bad.response.status", status))
      case errors: JsError         => errors
    }
  }

  implicit val PceResponseWrites: Writes[PceResponse] = Writes {
    case PathComputationConfirmed(correlationId, segments) => Json.obj("correlationId" -> correlationId, "status" -> "SUCCESS", "path" -> segments)
    case PathComputationFailed(correlationId, error)     => Json.obj("correlationId" -> correlationId, "status" -> "FAILED", "m.findPathError" -> Json.toJson(error))
  }

  implicit val PathComputationConfirmedReads: Reads[PathComputationConfirmed] = (
    (__ \ "correlationId").read[CorrelationId] and
    (__ \ "path").read[Seq[ComputedSegment]])(PathComputationConfirmed.apply _)

  private case class NsiErrorVariable(namespace: Option[String], name: String, value: String)

  private implicit val NsiErrorVariableReads: Reads[NsiErrorVariable] = (
    (__ \ "@namespace").readNullable[String] and
    (__ \ "@type").read[String] and
    (__ \ "value").read[String]) { NsiErrorVariable }

  private implicit val NsiErrorVariableWrites: Writes[NsiErrorVariable] = (
    (__ \ "@namespace").writeNullable[String] and
    (__ \ "@type").write[String] and
    (__ \ "value").write[String]) (unlift(NsiErrorVariable.unapply))

  private implicit val NsiErrorReads: Reads[NsiError] = (
    (__ \ "code").read[String] and
    (__ \ "label").read[String] and
    (__ \ "description").read[String] and
    (__ \ "variable").readNullable[NsiErrorVariable]) {
      (code, label, text, variable) => NsiError(code, label, text, variables = variable.map(v => new QName(v.namespace getOrElse XMLConstants.NULL_NS_URI, v.name) -> v.value).toSeq)
    }

  private implicit val NsiErrorWrites: Writes[NsiError] = Writes { (nsiError: NsiError) =>
    JsObject(
      (Seq[(String, JsValue)]()
        :+ "code" -> JsString(nsiError.id)
        :+ "label" -> JsString(nsiError.description)
        :+ "description" -> JsString(nsiError.text)
      ) ++ nsiError.variables.headOption.map {
          case (k, v) =>
            "variable" -> Json.toJson(NsiErrorVariable(
              Option(k.getNamespaceURI).filter(_.nonEmpty),
              k.getLocalPart,
              v
            ))
        }
    )
  }

  implicit val PathComputationFailedReads: Reads[PathComputationFailed] = (
    (__ \ "correlationId").read[CorrelationId] and
    (__ \ "message").read[String].map(s => NsiError.TopologyError.copy(text = s))
      .orElse((__ \ "m.findPathError").read[NsiError])) {
    (correlationId, error) =>
      PathComputationFailed(correlationId, error)
  }

  implicit val XMLGregorianCalendarReads: Reads[XMLGregorianCalendar] = Reads {
    case JsString(s) => Try { DatatypeFactory.newInstance().newXMLGregorianCalendar(s) }.map { x => JsSuccess(x) }.getOrElse { JsError(JsonValidationError("bad.timestamp", s)) }
    case json        => JsError(JsonValidationError("bad.timestamp", json))
  }
  implicit val XMLGregorianCalendarWrites: Writes[XMLGregorianCalendar] = Writes { x => JsString(x.toString) }

  implicit val ConnectionTypeFormat: Format[ConnectionType] = (
      (__ \ "index").format[Int] and
      (__ \ "value").format[String]).apply((index, value) => new ConnectionType().withIndex(index).withValue(value), connectionType => (connectionType.getIndex(), connectionType.getValue()))

  implicit val PceRequestFormat: Format[PceRequest] = (
    (__ \ "correlationId").format[CorrelationId] and
    (__ \ "nsaId").formatNullable[String] and
    (__ \ "replyTo" \ "url").format[URI] and
    (__ \ "replyTo" \ "mediaType").format[String] and
    (__ \ "algorithm").format[PathComputationAlgorithm] and
    (__ \ "startTime").formatNullable[XMLGregorianCalendar] and
    (__ \ "endTime").formatNullable[XMLGregorianCalendar] and
    (__ \ "constraints").format[Seq[String]] and
    ServiceTypeFormat and
    (__ \ "trace").format[Seq[ConnectionType]])
    .apply((correlationId, nsaId, replyTo, mediaType, algorithm, start, end, constraints, serviceType, connectionTrace) => {
      PathComputationRequest(correlationId, nsaId, replyTo, start.map(_.toInstant), end.map(_.toInstant), serviceType, algorithm, connectionTrace.toList)
    }, {
      case request: PathComputationRequest =>
        ( request.correlationId,
          request.nsaId,
          request.replyTo,
          "application/json",
          request.algorithm,
          request.startTime.map(_.toXMLGregorianCalendar()),
          request.endTime.map(_.toXMLGregorianCalendar()),
          Nil,
          request.serviceType,
          request.connectionTrace)
    })

  implicit val PceAcknowledgementWrites: Writes[PceAcknowledgement] = Writes {
    case PceAccepted(correlationId) => Json.obj("correlationId" -> correlationId, "status" -> 202)
    case pceFailed: PceFailed       => Json.writes[PceFailed].writes(pceFailed)
  }

  implicit val PceAcknowledgementReads: Reads[PceAcknowledgement] = Reads { json =>
    (__ \ "status").read[Int].reads(json) match {
      case JsSuccess(202, _)    => Json.fromJson[PceAccepted](json)
      case JsSuccess(status, _) => Json.fromJson[PceFailed](json)
      case errors: JsError      => errors
    }
  }
  private implicit val PceAcceptedReads: Reads[PceAccepted] = (
    (__ \ "correlationId").read[CorrelationId] and
    (__ \ "status").read[Int]) { (correlationId, status) => PceAccepted(correlationId) }
  private implicit val PceFailedReads: Reads[PceFailed] = Json.reads[PceFailed]
}
