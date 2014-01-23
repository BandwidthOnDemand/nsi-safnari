package nl.surfnet.safnari

import java.net.URI
import play.api.libs.json._
import play.api.libs.functional.syntax._
import java.util.UUID
import play.api.data.validation.ValidationError
import scala.util.Try
import javax.xml.datatype.XMLGregorianCalendar
import javax.xml.datatype.DatatypeFactory
import org.ogf.schemas.nsi._2013._12.connection.types._
import org.ogf.schemas.nsi._2013._12.services.types.DirectionalityType
import org.ogf.schemas.nsi._2013._12.services.point2point.P2PServiceBaseType

case class ServiceType(serviceType: String, service: P2PServiceBaseType)

sealed trait PceMessage {
  final def action = this.getClass.getSimpleName
  def correlationId: CorrelationId
}
sealed trait PceRequest extends PceMessage
case class PathComputationRequest(correlationId: CorrelationId, replyTo: URI, schedule: ScheduleType, serviceType: ServiceType) extends PceRequest

sealed trait PceResponse extends PceMessage
case class PathComputationFailed(correlationId: CorrelationId, message: String) extends PceResponse
case class PathComputationConfirmed(correlationId: CorrelationId, segments: Seq[ComputedSegment]) extends PceResponse

sealed trait PceAcknowledgement extends PceMessage
case class PceAccepted(correlationId: CorrelationId) extends PceAcknowledgement
case class PceFailed(correlationId: CorrelationId, status: Int, statusText: String, message: String) extends PceAcknowledgement

sealed trait ProviderAuthentication
case object NoAuthentication extends ProviderAuthentication
case class BasicAuthentication(username: String, password: String) extends ProviderAuthentication {
  override def toString = s"BasicAuthentication($username, *******)"
}
case class OAuthAuthentication(token: String) extends ProviderAuthentication {
  override def toString = "OAuthAuthentication(*******)"
}

case class ProviderEndPoint(nsa: String, url: URI, authentication: ProviderAuthentication)

case class ComputedSegment(provider: ProviderEndPoint, serviceType: ServiceType)

object PceMessage {
  private implicit class JsResultOps[A](js: JsResult[A]) {
    def clearPath = js.fold(JsError(_), JsSuccess(_, path = JsPath()))
  }

  implicit val CorrelationIdReads: Reads[CorrelationId] = Reads[CorrelationId] {
    case JsString(s) => CorrelationId.fromString(s).map { x => JsSuccess(x) }.getOrElse { JsError(ValidationError("bad.correlation.id", s)) }
    case json        => JsError(ValidationError("bad.correlation.id", json))
  }
  implicit val CorrelationIdWrites: Writes[CorrelationId] = Writes { x => JsString(x.toString) }

  implicit val ProviderAuthenticationReads: Reads[ProviderAuthentication] = Reads { json =>
    (__ \ "method").read[String].reads(json) match {
      case JsSuccess("NONE", _)    => JsSuccess(NoAuthentication)
      case JsSuccess("BASIC", _)   => Json.reads[BasicAuthentication].reads(json).clearPath
      case JsSuccess("OAUTH2", _)  => Json.reads[OAuthAuthentication].reads(json).clearPath
      case JsSuccess(method, path) => JsError(path -> ValidationError("bad.authentication.method", method))
      case errors: JsError         => errors
    }
  }
  implicit val ProviderAuthenticationWrites: Writes[ProviderAuthentication] = Writes {
    case NoAuthentication                        => Json.obj("method" -> "NONE")
    case BasicAuthentication(username, password) => Json.obj("method" -> "BASIC", "username" -> username, "password" -> password)
    case OAuthAuthentication(token)              => Json.obj("method" -> "OAUTH2", "token" -> token)
  }
  implicit val ProviderEndPointFormat: OFormat[ProviderEndPoint] = (
    (__ \ "nsa").format[String] and
    (__ \ "csProviderURL").format[URI] and
    (__ \ "credentials").format[ProviderAuthentication])(ProviderEndPoint.apply, unlift(ProviderEndPoint.unapply))

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
    case PathComputationFailed(correlationId, message)     => Json.obj("correlationId" -> correlationId, "status" -> "FAILED", "message" -> message)
  }

  implicit val PathComputationConfirmedReads: Reads[PathComputationConfirmed] = (
    (__ \ "correlationId").read[CorrelationId] and
    (__ \ "path").read[Seq[ComputedSegment]])(PathComputationConfirmed.apply _)

  implicit val PathComputationFailedReads: Reads[PathComputationFailed] = (
    (__ \ "correlationId").read[CorrelationId] and
    (__ \ "message").readNullable[String]) { (correlationId, optionalMessage) =>
      PathComputationFailed(correlationId, optionalMessage.getOrElse("no error message received from PCE"))
    }

  implicit val XMLGregorianCalendarReads: Reads[XMLGregorianCalendar] = Reads {
    case JsString(s) => Try { DatatypeFactory.newInstance().newXMLGregorianCalendar(s) }.map { x => JsSuccess(x) }.getOrElse { JsError(ValidationError("bad.timestamp", s)) }
    case json        => JsError(ValidationError("bad.timestamp", json))
  }
  implicit val XMLGregorianCalendarWrites: Writes[XMLGregorianCalendar] = Writes { x => JsString(x.toString) }

  implicit val PceRequestFormat: Format[PceRequest] = (
    (__ \ "correlationId").format[CorrelationId] and
    (__ \ "replyTo" \ "url").format[URI] and
    (__ \ "replyTo" \ "mediaType").format[String] and
    (__ \ "algorithm").formatNullable[String] and
    (__ \ "startTime").formatNullable[XMLGregorianCalendar] and
    (__ \ "endTime").formatNullable[XMLGregorianCalendar] and
    (__ \ "constraints").format[Seq[String]] and
    ServiceTypeFormat)
    .apply((correlationId, replyTo, mediaType, algorithm, start, end, constraints, serviceType) => {
      val schedule = new ScheduleType().withStartTime(start.orNull).withEndTime(end.orNull)
      PathComputationRequest(correlationId, replyTo, schedule, serviceType)
    }, {
      case PathComputationRequest(correlationId, replyTo, schedule, serviceType) =>
        (correlationId,
          replyTo,
          "application/json",
          None,
          Option(schedule.getStartTime()),
          Option(schedule.getEndTime()),
          Nil,
          serviceType)
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
