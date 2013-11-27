package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._07.framework.types.ServiceExceptionType

class IdempotentProvider(providerNsa: String, wrapped: InboundMessage => Option[Seq[OutboundMessage]]) extends (InboundMessage => Either[ServiceExceptionType, (Boolean, Seq[OutboundMessage])]) {
  private var fromRequester = Map.empty[CorrelationId, (FromRequester, Option[ToRequester])]
  private var fromRequesterByDownstreamCorrelationId = Map.empty[CorrelationId, FromRequester]
  private var downstreamRequestsByFromRequesterCorrelationId = Map.empty[CorrelationId, Map[CorrelationId, OutboundMessage]].withDefaultValue(Map.empty)

  override def apply(message: InboundMessage) = message match {
    case inbound @ FromRequester(NsiProviderMessage(_, _: NsiProviderCommand)) =>
      fromRequester.get(inbound.correlationId) match {
        case None =>
          val result = wrapped(inbound)
          result.foreach(recordOutput(inbound, _))
          result.map((false, _)).toRight(messageNotApplicable(inbound))
        case Some((original, result)) =>
          if (!sameMessage(inbound.message, original.message)) {
            Left(NsiError.PayloadError.toServiceException(providerNsa).withText(s"duplicate request with existing correlation id ${inbound.correlationId} does not match the original"))
          } else {
            Right((true, result.fold {
              val downstreamMessages = downstreamRequestsByFromRequesterCorrelationId(inbound.correlationId)
              downstreamMessages.values.to[List]
            } { reply =>
              List(reply)
            }))
          }
      }
    case _: FromProvider | _: FromPce =>
      val originalRequest = fromRequesterByDownstreamCorrelationId.get(message.correlationId)
      val result = originalRequest match {
        case None =>
          wrapped(message)
        case Some(originalRequest) =>
          val result = wrapped(message)
          result.foreach { output =>
            recordOutput(originalRequest, output)
            downstreamRequestsByFromRequesterCorrelationId += originalRequest.correlationId -> (downstreamRequestsByFromRequesterCorrelationId(originalRequest.correlationId) - message.correlationId)
          }
          result
      }
      result.map((false, _)).toRight(messageNotApplicable(message))
    case _: MessageDeliveryFailure | _: PassedEndTime | _: AckFromProvider | _: FromRequester =>
      val result = wrapped(message)
      result.map((false, _)).toRight(messageNotApplicable(message))
  }

  private def recordOutput(inbound: FromRequester, output: Seq[OutboundMessage]): Unit = {
    val toRequester = output.collectFirst {
      case outbound @ ToRequester(NsiRequesterMessage(_, _: NsiCommandReply)) =>
        assert(inbound.correlationId == outbound.correlationId, s"reply correlationId ${outbound.correlationId} did not match request correlationId ${inbound.correlationId}")
        outbound
    }
    var outboundMessagesByCorrelationId = output.foldLeft(Map.empty[CorrelationId, OutboundMessage]) { (acc, message) => acc + (message.correlationId -> message) }
    output.foreach { downstream =>
      downstream match {
        case _: ToPce | _: ToProvider =>
          fromRequesterByDownstreamCorrelationId += downstream.correlationId -> inbound
          downstreamRequestsByFromRequesterCorrelationId += inbound.correlationId -> (downstreamRequestsByFromRequesterCorrelationId(inbound.correlationId) + (downstream.correlationId -> downstream))
        case _: ToRequester =>
        // Ignore
      }
    }
    fromRequester += inbound.correlationId -> (inbound -> toRequester)
  }

  private def sameMessage(a: NsiProviderMessage[NsiProviderOperation], b: NsiProviderMessage[NsiProviderOperation]): Boolean = {
    // JAXB documents cannot be compared directly due to broken equals implementation of the DOM tree.
    // Serialize both messages to XML and compare the resulting strings instead.
    import NsiSoapConversions._
    val conversion = NsiProviderMessageToDocument[NsiProviderOperation](None).andThen(DocumentToString)
    conversion(a) == conversion(b)
  }

  private def messageNotApplicable(message: InboundMessage): ServiceExceptionType = NsiError.InvalidTransition.toServiceException(providerNsa)
}
