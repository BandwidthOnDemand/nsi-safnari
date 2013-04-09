package models

sealed trait NsiRequestMessage

object NsiRequestMessage {
  case class Reserve(correlationId: String) extends NsiRequestMessage
  case class Provision(correlationId: String) extends NsiRequestMessage
  case class QuerySummary(correlationId: String) extends NsiRequestMessage
}