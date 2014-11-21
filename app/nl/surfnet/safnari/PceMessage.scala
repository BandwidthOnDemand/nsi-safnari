package nl.surfnet.safnari

import java.net.URI
import javax.xml.datatype.{DatatypeFactory, XMLGregorianCalendar}

import net.nordu.namespaces._2013._12.gnsbod.ConnectionType
import nl.surfnet.nsiv2.messages._
import org.ogf.schemas.nsi._2013._12.connection.types._
import org.ogf.schemas.nsi._2013._12.services.point2point.P2PServiceBaseType
import org.ogf.schemas.nsi._2013._12.services.types.DirectionalityType
import play.api.data.validation.ValidationError
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.util.Try

case class ServiceType(serviceType: String, service: P2PServiceBaseType)

sealed trait PceMessage {
  final def action = this.getClass.getSimpleName
  def correlationId: CorrelationId
}
sealed trait PceRequest extends PceMessage
case class PathComputationRequest(correlationId: CorrelationId, replyTo: URI, schedule: ScheduleType, serviceType: ServiceType, algorithm: PathComputationAlgorithm, connectionTrace: List[ConnectionType]) extends PceRequest

sealed trait PathComputationAlgorithm { val name: String }
case object ChainAlgorithm extends PathComputationAlgorithm { val name = "CHAIN" }
case object TreeAlgorithm extends PathComputationAlgorithm { val name = "TREE" }
object PathComputationAlgorithm {
  val values: List[PathComputationAlgorithm] = List(ChainAlgorithm, TreeAlgorithm)
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
    case JsString(s) => CorrelationId.fromString(s).map { x => JsSuccess(x) }.getOrElse { JsError(ValidationError("bad.correlation.id", s)) }
    case json        => JsError(ValidationError("bad.correlation.id", json))
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

  implicit val ProviderEndPointFormat: OFormat[ProviderEndPoint] = (
    (__ \ "nsa").format[String] and
    (__ \ "csProviderURL").format[URI])(ProviderEndPoint.apply, unlift(ProviderEndPoint.unapply))

  implicit val pointToPointServiceFormat: OFormat[P2PServiceBaseType] = (
    (__ \ "capacity").format[Long] and
    (__ \ "directionality").formatNullable[String].inmap[Option[DirectionalityType]](_.map(DirectionalityType.fromValue(_)), _.map(_.value)) and
    (__ \ "symmetricPath").formatNullable[Boolean] and
    (__ \ "sourceSTP").format[String] and
    (__ \ "destSTP").format[String]
  ).apply((capacity, directionality, symmetricPath, source, dest) => {
    new P2PServiceBaseType()
      .withCapacity(capacity)
      .withDirectionality(directionality.getOrElse(DirectionalityType.BIDIRECTIONAL))
      .withSymmetricPath(symmetricPath.map(x => x: java.lang.Boolean).orNull)
      .withSourceSTP(source)
      .withDestSTP(dest)
  }, { p2ps => (p2ps.getCapacity(), Option(p2ps.getDirectionality()), Option(p2ps.isSymmetricPath()).map(_.booleanValue()), p2ps.getSourceSTP(), p2ps.getDestSTP()) })

  implicit val ServiceTypeFormat: OFormat[ServiceType] = (
    (__ \ "serviceType").format[String] and
    (__ \ "p.p2ps").format[Seq[P2PServiceBaseType]]).apply((st, p2ps) => ServiceType(st, p2ps.head), serviceType => (serviceType.serviceType, serviceType.service :: Nil))

  implicit val ComputedSegmentFormat: Format[ComputedSegment] = (ProviderEndPointFormat and ServiceTypeFormat)(ComputedSegment.apply, unlift(ComputedSegment.unapply))

  implicit val PceResponseReads: Reads[PceResponse] = Reads { json =>
    (__ \ "status").read[String].reads(json) match {
      case JsSuccess("SUCCESS", _) => Json.fromJson[PathComputationConfirmed](json)
      case JsSuccess("FAILED", _)  => Json.fromJson[PathComputationFailed](json)
      case JsSuccess(status, path) => JsError(path -> ValidationError("bad.response.status", status))
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

  implicit val NsiErrorVariableReads: Reads[NsiErrorVariable] = (
    (__ \ "@type").read[String] and
    (__ \ "value").read[String]) { NsiErrorVariable }

  implicit val NsiErrorVariableWrites: Writes[NsiErrorVariable] = (
    (__ \ "@type").write[String] and
    (__ \ "value").write[String]) (unlift(NsiErrorVariable.unapply))

  implicit val NsiErrorReads: Reads[NsiError] = (
    (__ \ "code").read[String] and
    (__ \ "label").read[String] and
    (__ \ "description").read[String] and
    (__ \ "variable").readNullable[NsiErrorVariable]) {
      (code, label, text, variable) => NsiError(code, label, text, variable)
    }

  implicit val NsiErrorWrites: Writes[NsiError] = Writes { (nsiError: NsiError) =>
    JsObject(
      (Seq[(String, JsValue)]()
      :+ "code" -> JsString(nsiError.id)
      :+ "label" -> JsString(nsiError.description)
      :+ "description" -> JsString(nsiError.text))
      ++ nsiError.variable.map("variable" -> Json.toJson(_))
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
    case JsString(s) => Try { DatatypeFactory.newInstance().newXMLGregorianCalendar(s) }.map { x => JsSuccess(x) }.getOrElse { JsError(ValidationError("bad.timestamp", s)) }
    case json        => JsError(ValidationError("bad.timestamp", json))
  }
  implicit val XMLGregorianCalendarWrites: Writes[XMLGregorianCalendar] = Writes { x => JsString(x.toString) }

  implicit val ConnectionTypeFormat: Format[ConnectionType] = (
      (__ \ "index").format[Int] and
      (__ \ "value").format[String]).apply((index, value) => new ConnectionType().withIndex(index).withValue(value), connectionType => (connectionType.getIndex(), connectionType.getValue()))

  implicit val PceRequestFormat: Format[PceRequest] = (
    (__ \ "correlationId").format[CorrelationId] and
    (__ \ "replyTo" \ "url").format[URI] and
    (__ \ "replyTo" \ "mediaType").format[String] and
    (__ \ "algorithm").format[PathComputationAlgorithm] and
    (__ \ "startTime").formatNullable[XMLGregorianCalendar] and
    (__ \ "endTime").formatNullable[XMLGregorianCalendar] and
    (__ \ "constraints").format[Seq[String]] and
    ServiceTypeFormat and
    (__ \ "trace").format[Seq[ConnectionType]])
    .apply((correlationId, replyTo, mediaType, algorithm, start, end, constraints, serviceType, connectionTrace) => {
      val schedule = new ScheduleType().withStartTime(start.orNull).withEndTime(end.orNull)
      PathComputationRequest(correlationId, replyTo, schedule, serviceType, algorithm, connectionTrace.toList)
    }, {
      case PathComputationRequest(correlationId, replyTo, schedule, serviceType, algorithm, connectionTrace) =>
        (correlationId,
          replyTo,
          "application/json",
          algorithm,
          Option(schedule.getStartTime()),
          Option(schedule.getEndTime()),
          Nil,
          serviceType,
          connectionTrace)
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
