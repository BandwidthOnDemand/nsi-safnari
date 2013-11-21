package nl.surfnet.safnari

import java.net.URI
import org.ogf.schemas.nsi._2013._07.framework.types.ServiceExceptionType

object NsiHeaders {
  val ProviderProtocolVersion: URI = URI.create("application/vnd.ogf.nsi.cs.v2.provider+soap")
  val RequesterProtocolVersion: URI = URI.create("application/vnd.ogf.nsi.cs.v2.requester+soap")
}
case class NsiHeaders(correlationId: CorrelationId, requesterNSA: RequesterNsa, providerNSA: String, replyTo: Option[URI], protocolVersion: URI) {
  def forSyncAck: NsiHeaders = copy(replyTo = None)
  def forAsyncReply: NsiHeaders = copy(replyTo = None, protocolVersion = NsiHeaders.RequesterProtocolVersion)
}

sealed trait NsiMessage[+T] {
  def headers: NsiHeaders
  def body: T
  def correlationId: CorrelationId = headers.correlationId
}
final case class NsiProviderMessage[+T](headers: NsiHeaders, body: T) extends NsiMessage[T] {
  def ack(ack: NsiAcknowledgement = GenericAck()) = NsiProviderMessage(headers.forSyncAck.copy(protocolVersion = NsiHeaders.ProviderProtocolVersion), ack)
  def reply(reply: NsiRequesterOperation) = NsiRequesterMessage(headers.forAsyncReply, reply)
}
final case class NsiRequesterMessage[+T](headers: NsiHeaders, body: T) extends NsiMessage[T] {
  def ack(ack: NsiAcknowledgement = GenericAck()) = NsiRequesterMessage(headers.forSyncAck.copy(protocolVersion = NsiHeaders.RequesterProtocolVersion), ack)
}

final case class NsiError(id: String, description: String, text: String) {
  override def toString = s"$id: $description: $text"

  def toServiceException(nsaId: String) = new ServiceExceptionType().withErrorId(id).withText(text).withNsaId(nsaId)
}
object NsiError {
  val PayloadError = NsiError("100", "PAYLOAD_ERROR", "")
  val MissingParameter = NsiError("101", "MISSING_PARAMETER", "Invalid or missing parameter")
  val UnsupportedParameter = NsiError("102", "UNSUPPORTED_PARAMETER", "Parameter provided contains an unsupported value which MUST be processed")
  val NotImplemented = NsiError("103", "NOT_IMPLEMENTED", "This operation is not implemented yet")
  val VersionNotSupported = NsiError("104", "VERSION_NOT_SUPPORTED", "The service version requested in NSI header is not supported")

  val ConnectionError = NsiError("200", "", "")
  val InvalidTransition = NsiError("201", "INVALID_TRANSITION", "Connection state machine is in invalid state for received message")
  val ConnectionExists = NsiError("202", "CONNECTION_EXISTS", "Schedule already exists for connectionId")
  val ConnectionNonExistent = NsiError("203", "CONNECTION_NONEXISTENT", "Schedule does not exist for connectionId")
  val ConnectionGone = NsiError("204", "CONNECTION_GONE", "")
  val ConnectionCreateError = NsiError("205", "CONNECTION_CREATE_ERROR", "Failed to create connection (payload was ok, something went wrong)")

  val SecurityError = NsiError("300", "SECURITY_ERROR", "")
  val AuthenticationFailure = NsiError("301", "AUTHENTICATION_FAILURE", "")
  val Unauthorized = NsiError("302", "UNAUTHORIZED", "")

  val TopologyError = NsiError("400", "TOPOLOGY_ERROR", "")
  val UnknownStp = NsiError("401", "UNKNOWN_STP", "Could not find STP in topology database")
  val StpResolutionError = NsiError("402", "STP_RESOLUTION_ERROR", "Could not resolve STP to a managing NSA")
  val NoPathFound = NsiError("403", "NO_PATH_FOUND", "Path computation failed to resolve route for reservation")
  val VlanIdInterchangeNotSupported = NsiError("404", "VLANID_INTERCHANGE_NOT_SUPPORTED", "VlanId interchange not supported for requested path")

  val InternalError = NsiError("500", "INTERNAL_ERROR", "An internal error has caused a message processing failure")
  val InternalNrmError = NsiError("501", "INTERNAL_NRM_ERROR", "An internal NRM error has caused a message processing failure")

  val ResourceUnavailable = NsiError("600", "RESOURCE_UNAVAILABLE", "")
  val StpUnavailable = NsiError("601", "STP_UNAVAILABLE", "Specified STP already in use")
  val BandwidthUnavailable = NsiError("602", "BANDWIDTH_UNAVAILABLE", "Insufficient bandwdith available for reservation")

  val ChildError = NsiError("???", "CHILD_ERROR", "One or more children reported an error. See the child exceptions for details.")
}
