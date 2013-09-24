package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._07.connection.types._
import org.ogf.schemas.nsi._2013._07.services.types.StpType
import java.net.URI
import play.api.libs.json._
import play.api.libs.functional.syntax._
import java.util.UUID
import play.api.data.validation.ValidationError
import scala.util.Try
import javax.xml.datatype.XMLGregorianCalendar
import javax.xml.datatype.DatatypeFactory
import org.ogf.schemas.nsi._2013._07.services.point2point.P2PServiceBaseType
import org.ogf.schemas.nsi._2013._07.services.point2point.EthernetVlanType
import org.ogf.schemas.nsi._2013._07.services.types.DirectionalityType
import org.ogf.schemas.nsi._2013._07.services.point2point.EthernetBaseType

sealed trait PceMessage {
  def correlationId: CorrelationId
}
sealed trait PceRequest extends PceMessage
case class PathComputationRequest(correlationId: CorrelationId, replyTo: URI, schedule: ScheduleType, serviceType: String, service: P2PServiceBaseType) extends PceRequest

sealed trait PceResponse extends PceMessage
case class PathComputationFailed(correlationId: CorrelationId, message: String) extends PceResponse
case class PathComputationConfirmed(correlationId: CorrelationId, segments: Seq[ComputedSegment]) extends PceResponse

sealed trait ProviderAuthentication
case object NoAuthentication extends ProviderAuthentication
case class BasicAuthentication(username: String, password: String) extends ProviderAuthentication {
  // FIXME override toString to avoid printing password to log.
}
case class OAuthAuthentication(token: String) extends ProviderAuthentication {
  // FIXME override toString to avoid printing token to log.
}

case class ProviderEndPoint(nsa: String, url: URI, authentication: ProviderAuthentication)

case class ComputedSegment(service: P2PServiceBaseType, provider: ProviderEndPoint)

object PceMessage {
  private implicit class JsResultOps[A](js: JsResult[A]) {
    def clearPath = js.fold(JsError(_), JsSuccess(_, path = JsPath()))
  }

  implicit val CorrelationIdReads: Reads[CorrelationId] = Reads[CorrelationId] {
    case JsString(s) => CorrelationId.fromString(s).map { x => JsSuccess(x) }.getOrElse { JsError(ValidationError("bad.correlation.id", s)) }
    case json        => JsError(ValidationError("bad.correlation.id", json))
  }
  implicit val CorrelationIdWrites: Writes[CorrelationId] = Writes { x => JsString(x.toString) }

  implicit val UriReads: Reads[URI] = Reads[URI] {
    case JsString(s) => Try { new URI(s) }.map { x => JsSuccess(x) }.getOrElse { JsError(ValidationError("bad.uri", s)) }
    case json        => JsError(ValidationError("bad.uri", json))
  }
  implicit val UriWrites: Writes[URI] = Writes[URI] { x => JsString(x.toASCIIString()) }

  implicit val StpTypeFormat: Format[StpType] = (
    (__ \ "networkId").format[String] and
    (__ \ "localId").format[String])(
      (networkId, localId) => new StpType().withNetworkId(networkId).withLocalId(localId),
      stpType => (stpType.getNetworkId(), stpType.getLocalId()))

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
    (__ \ "providerUrl").format[URI] and
    (__ \ "auth").format[ProviderAuthentication])(ProviderEndPoint.apply, unlift(ProviderEndPoint.unapply))

  implicit val EthernetVlanTypeFormat: OFormat[EthernetVlanType] = (
    (__ \ "capacity").format[Long] and
    (__ \ "directionality").formatNullable[String].inmap[DirectionalityType](_.map(DirectionalityType.fromValue(_)).getOrElse(DirectionalityType.BIDIRECTIONAL), x => Some(x.value)) and
    (__ \ "symmetricPath").format[Boolean] and
    (__ \ "sourceSTP").format[StpType] and
    (__ \ "destSTP").format[StpType] and
    (__ \ "mtu").formatNullable[Int] and
    (__ \ "burstsize").formatNullable[Long] and
    (__ \ "sourceVLAN").format[Int] and
    (__ \ "destVLAN").format[Int]
  )((capacity, directionality, symmetricPath, source, dest, mtu, burstsize, sourceVlan, destVlan) =>
      new EthernetVlanType()
        .withCapacity(capacity)
        .withDirectionality(directionality)
        .withSymmetricPath(symmetricPath)
        .withSourceSTP(source)
        .withDestSTP(dest)
        .withMtu(mtu.orNull)
        .withBurstsize(burstsize.orNull)
        .withSourceVLAN(sourceVlan)
        .withDestVLAN(destVlan),
    p2p => (p2p.getCapacity(), p2p.getDirectionality(), p2p.isSymmetricPath(), p2p.getSourceSTP(), p2p.getDestSTP(), Option(p2p.getMtu()), Option(p2p.getBurstsize()), p2p.getSourceVLAN(), p2p.getDestVLAN()))

  implicit val EthernetBaseTypeFormat: OFormat[EthernetBaseType] = (
    (__ \ "capacity").format[Long] and
    (__ \ "directionality").formatNullable[String].inmap[DirectionalityType](_.map(DirectionalityType.fromValue(_)).getOrElse(DirectionalityType.BIDIRECTIONAL), x => Some(x.value)) and
    (__ \ "symmetricPath").format[Boolean] and
    (__ \ "sourceSTP").format[StpType] and
    (__ \ "destSTP").format[StpType] and
    (__ \ "mtu").formatNullable[Int] and
    (__ \ "burstsize").formatNullable[Long]
  )((capacity, directionality, symmetricPath, source, dest, mtu, burstsize) =>
      new EthernetBaseType()
        .withCapacity(capacity)
        .withDirectionality(directionality)
        .withSymmetricPath(symmetricPath)
        .withSourceSTP(source)
        .withDestSTP(dest)
        .withMtu(mtu.orNull)
        .withBurstsize(burstsize.orNull),
    p2p => (p2p.getCapacity(), p2p.getDirectionality(), p2p.isSymmetricPath(), p2p.getSourceSTP(), p2p.getDestSTP(), Option(p2p.getMtu()), Option(p2p.getBurstsize())))

  implicit val PointToPointServiceFormat: OFormat[P2PServiceBaseType] = (
    (__ \ "capacity").format[Long] and
    (__ \ "directionality").formatNullable[String].inmap[Option[DirectionalityType]](_.map(DirectionalityType.fromValue(_)), _.map(_.value)) and
    (__ \ "symmetricPath").formatNullable[Boolean] and
    (__ \ "sourceSTP").format[StpType] and
    (__ \ "destSTP").format[StpType]
  )((capacity, directionality, symmetricPath, source, dest) => {
      new P2PServiceBaseType()
        .withCapacity(capacity)
        .withDirectionality(directionality.getOrElse(DirectionalityType.BIDIRECTIONAL))
        .withSymmetricPath(symmetricPath.map(x => x: java.lang.Boolean).orNull)
        .withSourceSTP(source)
        .withDestSTP(dest)},
    p2p => (p2p.getCapacity(), Option(p2p.getDirectionality()), Option(p2p.isSymmetricPath()).map(_.booleanValue), p2p.getSourceSTP(), p2p.getDestSTP()))

  implicit val ComputedSegmentFormat: Format[ComputedSegment] = (
    PointToPointServiceFormat and
    ProviderEndPointFormat)(ComputedSegment.apply, unlift(ComputedSegment.unapply))

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
    (__ \ "path").read[Seq[ComputedSegment]]
  )(PathComputationConfirmed.apply _)

  implicit val PathComputationFailedReads: Reads[PathComputationFailed] = (
    (__ \ "correlationId").read[CorrelationId] and
    (__ \ "message").read[String]
  )(PathComputationFailed.apply _)

  implicit val XMLGregorianCalendarReads: Reads[XMLGregorianCalendar] = Reads {
    case JsString(s) => Try { DatatypeFactory.newInstance().newXMLGregorianCalendar(s) }.map { x => JsSuccess(x) }.getOrElse { JsError(ValidationError("bad.timestamp", s)) }
    case json        => JsError(ValidationError("bad.timestamp", json))
  }
  implicit val XMLGregorianCalendarWrites: Writes[XMLGregorianCalendar] = Writes { x => JsString(x.toString) }

  implicit val PceRequestWrites: Writes[P2PServiceBaseType] = Writes {
    case service: EthernetVlanType => Json.obj()
    case service: EthernetBaseType => ???
    case service: P2PServiceBaseType => ???
  }

  implicit val PceRequestFormat: Format[PceRequest] = (
    (__ \ "correlationId").format[CorrelationId] and
    (__ \ "replyTo" \ "url").format[URI] and
    (__ \ "replyTo" \ "mediaType").format[String] and
    (__ \ "algorithm").format[Option[String]] and
    (__ \ "startTime").format[Option[XMLGregorianCalendar]] and
    (__ \ "endTime").format[Option[XMLGregorianCalendar]] and
    (__ \ "serviceType").format[String] and
    (__ \ "constraints").format[Seq[String]] and
    (__ \ "p.pts").formatNullable[P2PServiceBaseType] and
    (__ \ "p.ets").formatNullable[EthernetBaseType] and
    (__ \ "p.evts").formatNullable[EthernetVlanType]
  ).apply((correlationId, replyTo, mediaType, algorithm, start, end, serviceType, constraints, pts, ets, evts) => {
      val schedule = new ScheduleType().withStartTime(start.orNull).withEndTime(end.orNull)
      PathComputationRequest(correlationId, replyTo, schedule, serviceType, pts.orElse(ets).orElse(evts).get)
    }, {
      case PathComputationRequest(correlationId, replyTo, schedule, serviceType, service) =>
        (correlationId,
          replyTo,
          "application/json",
          None,
          Option(schedule.getStartTime()),
          Option(schedule.getEndTime()),
          serviceType,
          Nil,
          service)
    })
}
