package nl.surfnet.safnari

import java.net.URI
import org.ogf.schemas.nsi._2013._12.framework.types._
import scala.collection.JavaConverters._
import org.ogf.schemas.nsi._2013._12.framework.headers.SessionSecurityAttrType
import net.nordu.namespaces._2013._12.gnsbod.ConnectionType

object NsiHeaders {
  val ProviderProtocolVersion: URI = URI.create("application/vnd.ogf.nsi.cs.v2.provider+soap")
  val RequesterProtocolVersion: URI = URI.create("application/vnd.ogf.nsi.cs.v2.requester+soap")
}

case class NsiHeaders(correlationId: CorrelationId, requesterNSA: RequesterNsa, providerNSA: String, replyTo: Option[URI], protocolVersion: URI, sessionSecurityAttrs: List[SessionSecurityAttrType] = Nil, connectionTrace: List[ConnectionType] = Nil) {
  def forSyncAck: NsiHeaders = copy(replyTo = None, sessionSecurityAttrs = Nil, connectionTrace = Nil)
  def forAsyncReply: NsiHeaders = copy(replyTo = None, protocolVersion = NsiHeaders.RequesterProtocolVersion, sessionSecurityAttrs = Nil, connectionTrace = Nil)
}

sealed trait NsiMessage[+T] {
  def headers: NsiHeaders
  def body: T
  def correlationId: CorrelationId = headers.correlationId
}

final case class NsiProviderMessage[+T](headers: NsiHeaders, body: T) extends NsiMessage[T] {
  def ack(acknowledgement: NsiAcknowledgement = GenericAck()): NsiProviderMessage[NsiAcknowledgement] = ack(headers, acknowledgement)

  def ackWithCorrectedProviderNsa(providerNsa: String, acknowledgement: NsiAcknowledgement = GenericAck()): NsiProviderMessage[NsiAcknowledgement] =
    ack(headers.copy(providerNSA = providerNsa), acknowledgement)

  private def ack(ackHeaders: NsiHeaders, ack: NsiAcknowledgement) =
    NsiProviderMessage(ackHeaders.forSyncAck.copy(protocolVersion = NsiHeaders.ProviderProtocolVersion), ack)

  def reply(reply: NsiRequesterOperation) = NsiRequesterMessage(headers.forAsyncReply, reply)
}

final case class NsiRequesterMessage[+T](headers: NsiHeaders, body: T) extends NsiMessage[T] {
  def ack(acknowledgement: NsiAcknowledgement = GenericAck()): NsiRequesterMessage[NsiAcknowledgement] = ack(headers, acknowledgement)

  def ackWithCorrectedRequesterNsa(requesterNsa: String, acknowledgement: NsiAcknowledgement = GenericAck()): NsiRequesterMessage[NsiAcknowledgement] =
    ack(headers.copy(requesterNSA = requesterNsa), acknowledgement)

  private def ack(ackHeaders: NsiHeaders, ack: NsiAcknowledgement) =
    NsiRequesterMessage(ackHeaders.forSyncAck.copy(protocolVersion = NsiHeaders.RequesterProtocolVersion), ack)
}

final case class NsiErrorVariable(name: String, value: String)

final case class NsiError(id: String, description: String, text: String, variable: Option[NsiErrorVariable]) {
  override def toString = s"$id: $description: $text"

  def toServiceException(nsaId: String, args: (String, String)*) = {
    val pairs = variable.toSeq.map( v => v.name -> v.value ) ++ args
    val variables = pairs map { case (t, v) => new TypeValuePairType().withType(t).withValue(v) }

    new ServiceExceptionType()
      .withErrorId(id)
      .withText(text)
      .withNsaId(nsaId)
      .withVariables(new VariablesType().withVariable(variables.asJava))
  }
}

object NsiError {
  def apply(id: String, description: String, text: String) = new NsiError(id, description, text, None)

  val PayloadError = NsiError("00100", "PAYLOAD_ERROR", "")
  val MissingParameter = NsiError("00101", "MISSING_PARAMETER", "Invalid or missing parameter")
  val UnsupportedParameter = NsiError("00102", "UNSUPPORTED_PARAMETER", "Parameter provided contains an unsupported value which MUST be processed")
  val NotImplemented = NsiError("00103", "NOT_IMPLEMENTED", "This operation is not implemented yet")
  val VersionNotSupported = NsiError("00104", "VERSION_NOT_SUPPORTED", "The service version requested in NSI header is not supported")

  val ConnectionError = NsiError("00200", "", "")
  val InvalidTransition = NsiError("00201", "INVALID_TRANSITION", "Connection state machine is in invalid state for received message")
  val ConnectionExists = NsiError("00202", "CONNECTION_EXISTS", "Schedule already exists for connectionId")
  val ConnectionNonExistent = NsiError("00203", "CONNECTION_NONEXISTENT", "Schedule does not exist for connectionId")
  val ConnectionGone = NsiError("00204", "CONNECTION_GONE", "")
  val ConnectionCreateError = NsiError("00205", "CONNECTION_CREATE_ERROR", "Failed to create connection (payload was ok, something went wrong)")

  val SecurityError = NsiError("00300", "SECURITY_ERROR", "")
  val AuthenticationFailure = NsiError("00301", "AUTHENTICATION_FAILURE", "")
  val Unauthorized = NsiError("00302", "UNAUTHORIZED", "")

  val TopologyError = NsiError("00400", "TOPOLOGY_ERROR", "")
  val UnknownStp = NsiError("00401", "UNKNOWN_STP", "Could not find STP in topology database")
  val StpResolutionError = NsiError("00402", "STP_RESOLUTION_ERROR", "Could not resolve STP to a managing NSA")
  val NoPathFound = NsiError("00403", "NO_PATH_FOUND", "Path computation failed to resolve route for reservation")
  val VlanIdInterchangeNotSupported = NsiError("00404", "VLANID_INTERCHANGE_NOT_SUPPORTED", "VlanId interchange not supported for requested path")

  val InternalError = NsiError("00500", "INTERNAL_ERROR", "An internal error has caused a message processing failure")
  val InternalNrmError = NsiError("00501", "INTERNAL_NRM_ERROR", "An internal NRM error has caused a message processing failure")

  val ResourceUnavailable = NsiError("00600", "RESOURCE_UNAVAILABLE", "")
  val StpUnavailable = NsiError("00601", "STP_UNAVAILABLE", "Specified STP already in use")
  val BandwidthUnavailable = NsiError("00602", "BANDWIDTH_UNAVAILABLE", "Insufficient bandwdith available for reservation")

  val ChildError = NsiError("???", "CHILD_ERROR", "One or more children reported an error. See the child exceptions for details.")
}
