package nl.surfnet.safnari

import nl.surfnet.nsiv2._
import nl.surfnet.nsiv2.messages._
import org.ogf.schemas.nsi._2013._12.framework.types.ServiceExceptionType

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

class IdempotentProvider(providerNsa: String, wrapped: InboundMessage => Either[ServiceExceptionType, Seq[OutboundMessage]]) extends (InboundMessage => Either[ServiceExceptionType, Seq[OutboundMessage]]) {
  private var requesterCommands = Map.empty[CorrelationId, RequesterCommandStatus]
  private var outgoingToRequesterCommands = Map.empty[CorrelationId, CorrelationId]

  override def apply(message: InboundMessage) = message match {
    case inbound @ FromRequester(NsiProviderMessage(_, _: NsiProviderCommand)) =>
      requesterCommands.get(inbound.correlationId) match {
        case None =>
          val result = wrapped(inbound)
          result.right.foreach(recordOutput(inbound, _))
          result
        case Some(RequesterCommandStatus(original, outstandingCommandsOrReply)) =>
          if (!sameMessage(inbound.message, original.message)) {
            Left(NsiError.PayloadError.toServiceException(providerNsa).withText(s"duplicate request with existing correlation id ${inbound.correlationId} does not match the original"))
          } else {
            Right(outstandingCommandsOrReply match {
              case Left(outstandingCommands) =>
                outstandingCommands.values.to[Seq]
              case Right(reply) =>
                List(reply)
            })
          }
      }
    case _: FromRequester =>
      wrapped(message)
    case _: FromProvider | _: FromPce | _: MessageDeliveryFailure | _: PassedEndTime | _: AckFromProvider | _: AckFromPce =>
      val output = wrapped(message)
      for {
        requesterCommandCorrelationId <- outgoingToRequesterCommands.get(message.correlationId)
        RequesterCommandStatus(originalRequest, _) <- requesterCommands.get(requesterCommandCorrelationId)
        messages <- output.right.toOption
      } {
        recordOutput(originalRequest, messages)
        message match {
          case _: FromProvider | _: FromPce =>
            requesterCommands += originalRequest.correlationId -> requesterCommands(originalRequest.correlationId).commandReplyReceived(message)
          case _ =>
        }
      }
      output
  }

  private def recordOutput(requesterCommand: FromRequester, output: Seq[OutboundMessage]): Unit = {
    val requesterReply = output.collectFirst {
      case reply @ ToRequester(NsiRequesterMessage(_, _: NsiCommandReply)) => reply
    }

    val newOutstandingCommands = output.collect {
      case message: ToPce      => message
      case message: ToProvider => message
    }
    outgoingToRequesterCommands ++= newOutstandingCommands.map { _.correlationId -> requesterCommand.correlationId }

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
    import soap.NsiSoapConversions._
    val conversion = NsiProviderMessageToDocument[NsiProviderOperation](None).andThen(DocumentToString)
    conversion(a) == conversion(b)
  }
}
