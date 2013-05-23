package nl.surfnet.safnari

import java.net.URI
import java.util.UUID
import org.ogf.schemas.nsi._2013._04.framework.types.ServiceExceptionType

case class NsiHeaders(correlationId: CorrelationId, requesterNSA: String, providerNSA: String, replyTo: Option[URI], protocolVersion: URI = URI.create("urn:nsi:2.0:FIXME")) {
  def asReply: NsiHeaders = copy(replyTo = None)
}

trait NsiMessage {
  def headers: NsiHeaders
  def correlationId: CorrelationId = headers.correlationId

  def ack = GenericAck(headers.asReply)
}
trait NsiQuery
trait NsiCommand

final case class NsiError(id: String, description: String, text: String) {
  override def toString = s"$id: $description: $text"

  def toServiceException(nsaId: String) = new ServiceExceptionType().withErrorId(id).withText(text).withNsaId(nsaId)
}
object NsiError {
  // TODO use errors from [http://code.google.com/p/ogf-nsi-project/wiki/NSIErrorCodes].

  val MissingParameter = NsiError("SVC0001", "MISSING_PARAMETER", "Invalid or missing parameter")
  val UnsupportedOperation = NsiError("SVC0002", "UNSUPPORTED_OPERATION", "Parameter provided contains an unsupported value which MUST be processed")
  val AlreadyExists = NsiError("SVC0003", "ALREADY_EXISTS", "Schedule already exists for connectionId")
  val DoesNotExist = NsiError("SVC0004", "DOES_NOT_EXIST", "Schedule does not exist for connectionId")
  val MissingSecurity = NsiError("SVC0005", "MISSING_SECURITY", "Invalid or missing user credentials")
  val TopologyResolutionStp = NsiError("SVC0006", "TOPOLOGY_RESOLUTION_STP", "Could not resolve STP in Topology database")
  val TopologyResolutionStpNsa = NsiError("SVC0007", "TOPOLOGY_RESOLUTION_STP_NSA", "Could not resolve STP to managing NSA")
  val PathComputationNoPath = NsiError("SVC0008", "PATH_COMPUTATION_NO_PATH", "Path computation failed to resolve route for reservation")
  val InvalidState = NsiError("SVC0009", "INVALID_STATE", "Connection state machine is in invalid state for received message")
  val InternalError = NsiError("SVC0010", "INTERNAL_ERROR", "An internal error has caused a message processing failure")
  val InternalNrmError = NsiError("SVC0011", "INTERNAL_NRM_ERROR", "An internal NRM error has caused a message processing failure")
  val StpAlreadyInUse = NsiError("SVC0012", "STP_ALREADY_IN_USE", "Specified STP already in use")
  val BandwidthNotAvailable = NsiError("SVC0012", "BANDWIDTH_NOT_AVAILABLE", "Insufficient bandwidth available for reservation")

  val ChildError = NsiError("SVC????", "CHILD_ERROR", "One or more children reported an error. See the child exceptions for details.")
}
