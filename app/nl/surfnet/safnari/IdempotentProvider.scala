package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._07.framework.types.ServiceExceptionType

private case class RequesterCommandStatus(command: FromRequester, outstandingCommandsOrReply: Either[Map[CorrelationId, OutboundMessage], ToRequester]) {
  def replySent(reply: ToRequester) = {
    assert(command.correlationId == reply.correlationId, s"reply $reply correlationId did not match command $command correlationId")
    copy(outstandingCommandsOrReply = Right(reply))
  }

  def commandsSent(commands: Seq[OutboundMessage]) = copy(outstandingCommandsOrReply = outstandingCommandsOrReply.left.map(_ ++ commands.map(m => m.correlationId -> m)))

  def commandReplyReceived(outstandingCommand: InboundMessage) = copy(outstandingCommandsOrReply = outstandingCommandsOrReply match {
    case Left(outstandingCommands) => Left(outstandingCommands - outstandingCommand.correlationId)
    case reply @ Right(_)          => reply
  })
}

class IdempotentProvider(providerNsa: String, wrapped: InboundMessage => Option[Seq[OutboundMessage]]) extends (InboundMessage => Either[ServiceExceptionType, (Boolean, Seq[OutboundMessage])]) {
  private var requesterCommands = Map.empty[CorrelationId, RequesterCommandStatus]
  private var outstandingToRequesterCommands = Map.empty[CorrelationId, CorrelationId]

  override def apply(message: InboundMessage) = message match {
    case inbound @ FromRequester(NsiProviderMessage(_, _: NsiProviderCommand)) =>
      requesterCommands.get(inbound.correlationId) match {
        case None =>
          val result = wrapped(inbound)
          result.foreach(recordOutput(inbound, _))
          result.map((false, _)).toRight(messageNotApplicable(inbound))
        case Some(RequesterCommandStatus(original, outstandingCommandsOrReply)) =>
          if (!sameMessage(inbound.message, original.message)) {
            Left(NsiError.PayloadError.toServiceException(providerNsa).withText(s"duplicate request with existing correlation id ${inbound.correlationId} does not match the original"))
          } else {
            Right((true, outstandingCommandsOrReply match {
              case Left(outstandingCommands) =>
                outstandingCommands.values.to[Seq]
              case Right(reply) =>
                List(reply)
            }))
          }
      }
    case _: FromProvider | _: FromPce =>
      val result = outstandingToRequesterCommands.get(message.correlationId).flatMap(requesterCommands.get) match {
        case None =>
          wrapped(message)
        case Some(RequesterCommandStatus(originalRequest, _)) =>
          outstandingToRequesterCommands -= message.correlationId
          val output = wrapped(message)
          output.foreach { messages =>
            recordOutput(originalRequest, messages)
            requesterCommands += originalRequest.correlationId -> requesterCommands(originalRequest.correlationId).commandReplyReceived(message)
          }
          output
      }
      result.map((false, _)).toRight(messageNotApplicable(message))
    case _: MessageDeliveryFailure | _: PassedEndTime | _: AckFromProvider | _: FromRequester | _: AckFromPce =>
      val result = wrapped(message)
      result.map((false, _)).toRight(messageNotApplicable(message))
  }

  private def recordOutput(requesterCommand: FromRequester, output: Seq[OutboundMessage]): Unit = {
    val requesterReply = output.collectFirst {
      case reply @ ToRequester(NsiRequesterMessage(_, _: NsiCommandReply)) => reply
    }

    val newOutstandingCommands = output.collect {
      case message: ToPce      => message
      case message: ToProvider => message
    }
    outstandingToRequesterCommands ++= newOutstandingCommands.map { _.correlationId -> requesterCommand.correlationId }

    val currentStatus = requesterCommands.getOrElse(requesterCommand.correlationId, RequesterCommandStatus(requesterCommand, Left(Map.empty)))

    val updatedStatus = requesterReply match {
      case None =>
        currentStatus.commandsSent(newOutstandingCommands)
      case Some(reply) =>
        currentStatus.replySent(reply)
    }
    requesterCommands = requesterCommands.updated(requesterCommand.correlationId, updatedStatus)
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
