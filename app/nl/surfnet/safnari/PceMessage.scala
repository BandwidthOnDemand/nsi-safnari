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

case class ServiceType(serviceType: String, service: P2PServiceBaseType)

sealed trait PceMessage {
  def correlationId: CorrelationId
}
sealed trait PceRequest extends PceMessage
case class PathComputationRequest(correlationId: CorrelationId, replyTo: URI, schedule: ScheduleType, serviceType: ServiceType) extends PceRequest

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
    (__ \ "csProviderURL").format[URI] and
    (__ \ "credentials").format[ProviderAuthentication])(ProviderEndPoint.apply, unlift(ProviderEndPoint.unapply))

  // Formats for the P2PServiceBaseType hierarchy. This is rather involved due to the use of sub-typing in the XML mappings.
  private def pointToPointServiceFormat[A <: P2PServiceBaseType](f: => A = new P2PServiceBaseType()): OFormat[A] = (
    (__ \ "capacity").format[Long] and
    (__ \ "directionality").formatNullable[String].inmap[Option[DirectionalityType]](_.map(DirectionalityType.fromValue(_)), _.map(_.value)) and
    (__ \ "symmetricPath").formatNullable[Boolean] and
    (__ \ "sourceSTP").format[StpType] and
    (__ \ "destSTP").format[StpType]).apply(
      { (capacity, directionality, symmetricPath, source, dest) =>
        val p2ps = f
        p2ps.setCapacity(capacity)
        p2ps.setDirectionality(directionality.getOrElse(DirectionalityType.BIDIRECTIONAL))
        p2ps.setSymmetricPath(symmetricPath.map(x => x: java.lang.Boolean).orNull)
        p2ps.setSourceSTP(source)
        p2ps.setDestSTP(dest)
        p2ps
      }, { p2ps =>
        (p2ps.getCapacity(), Option(p2ps.getDirectionality()), Option(p2ps.isSymmetricPath()).map(_.booleanValue()), p2ps.getSourceSTP(), p2ps.getDestSTP())
      })

  private def ethernetBaseTypeFormat[A <: EthernetBaseType](f: => A = new EthernetBaseType()): OFormat[A] = (
    pointToPointServiceFormat(f) and
    (__ \ "mtu").formatNullable[Int] and
    (__ \ "burstsize").formatNullable[Long])(
      { (ets, mtu, burstsize) =>
        ets.setMtu(mtu.map(x => x: java.lang.Integer).orNull)
        ets.setBurstsize(burstsize.map(x => x: java.lang.Long).orNull)
        ets
      }, { ets =>
        (ets, Option(ets.getMtu()).map(_.toInt), Option(ets.getBurstsize()).map(_.toLong))
      })

  private def ethernetVlanTypeFormat[A <: EthernetVlanType](f: => A = new EthernetVlanType()): OFormat[A] = (
    ethernetBaseTypeFormat(f) and
    (__ \ "sourceVLAN").format[Int] and
    (__ \ "destVLAN").format[Int]).apply(
      { (evts, sourceVlan, destVlan) =>
        evts.setSourceVLAN(sourceVlan)
        evts.setDestVLAN(destVlan)
        evts
      }, { evts =>
        (evts, evts.getSourceVLAN(), evts.getDestVLAN())
      })

  private val ServiceTypeReads: Reads[ServiceType] = (
    (__ \ "serviceType").read[String] and
    // Explicitly pass in the required formats. Inference doesn't work due to sub-typing.
    ((__ \ "p.p2ps").read(Reads.seq(pointToPointServiceFormat())) or
      (__ \ "p.ets").read(Reads.seq(ethernetBaseTypeFormat())).map(identity[Seq[P2PServiceBaseType]]) or
      (__ \ "p.evts").read(Reads.seq(ethernetVlanTypeFormat())).map(identity[Seq[P2PServiceBaseType]])).filter(ValidationError("error.single.service.required"))(_.size == 1)) {
        (serviceType, services) => ServiceType(serviceType, services.head)
      }

  private val ServiceTypeWrites: OWrites[ServiceType] = OWrites {
    case ServiceType(serviceType, service: EthernetVlanType)   => Json.obj("serviceType" -> serviceType, "p.evts" -> Json.arr(ethernetVlanTypeFormat().writes(service)))
    case ServiceType(serviceType, service: EthernetBaseType)   => Json.obj("serviceType" -> serviceType, "p.ets" -> Json.arr(ethernetBaseTypeFormat().writes(service)))
    case ServiceType(serviceType, service: P2PServiceBaseType) => Json.obj("serviceType" -> serviceType, "p.p2ps" -> Json.arr(pointToPointServiceFormat().writes(service)))
  }

  implicit val ServiceTypeFormat = OFormat(ServiceTypeReads, ServiceTypeWrites)

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
}
