package nl.surfnet.nsiv2.messages

import java.util.UUID

import scala.util.Try

case class CorrelationId private (value: UUID) {
  override def toString = s"${CorrelationId.Prefix}${value}"
}
object CorrelationId {
  val Prefix = "urn:uuid:"

  def apply(mostSigBits: Long, leastSigBits: Long) = fromUuid(new UUID(mostSigBits, leastSigBits))

  def fromUuid(uuid: UUID): CorrelationId = CorrelationId(uuid)

  def fromString(s: String): Option[CorrelationId] = s.splitAt(Prefix.size) match {
    case (Prefix, uuid) => Try { fromUuid(UUID.fromString(uuid)) }.toOption
    case _              => None
  }
}
