package nl.surfnet

import java.util.UUID
package object nsi {
  type Message = Any
  type ConnectionId = String
  type CorrelationId = UUID

  def newConnectionId = UUID.randomUUID.toString
  def newCorrelationId = UUID.randomUUID
}
