package nl.surfnet.safnari

import java.net.URI
import java.util.UUID

case class NsiEnvelope[T](headers: NsiHeaders, body: T) {
  def replyTo = headers.replyTo
}

case class NsiHeaders(correlationId: CorrelationId, requesterNSA: String, providerNSA: String, replyTo: Option[URI], protocolVersion: URI = URI.create("urn:nsi:2.0:FIXME")) {
  def asReply: NsiHeaders = copy(replyTo = None)
}

trait NsiMessage {
  def correlationId: CorrelationId
}
trait NsiQuery
trait NsiCommand
