package nl.surfnet.safnari

import scala.concurrent.Future

final case class HandlerResult[S, D](name: S, data: D, reply: Option[Any] = None) {
  def using(nextData: D) = copy(data = nextData)
  def replying(message: Any) = copy(reply = Some(message))
}
final case class Event[D](message: Any, data: D)

abstract class FiniteStateMachine[S, D](initialStateName: S, initialStateData: D) {
  type StateName = S
  type StateData = D
  type State = HandlerResult[S, D]

  private var _stateName: S = initialStateName
  private var _stateData: D = initialStateData

  def stateName = _stateName
  protected def stateData = _stateData

  private var _nextStateName: S = stateName
  private var _nextStateData: D = stateData

  protected def nextStateName = _nextStateName
  protected def nextStateData = _nextStateData

  type Handler = PartialFunction[Event[D], State]
  private var handlers: Map[S, Handler] = Map.empty
  private var unhandled: Handler = PartialFunction.empty
  private var transitionHandler: PartialFunction[(S, S), Unit] = { case _ => () }

  protected def when(stateName: StateName)(handler: Handler) = {
    require(!handlers.contains(stateName), s"handler for state $stateName already defined")
    handlers += stateName -> handler
  }
  protected def whenUnhandled(handler: PartialFunction[Event[D], State]): Unit = {
    unhandled = handler
  }
  protected def onTransition(handler: PartialFunction[(S, S), Unit]): Unit = {
    transitionHandler = handler
  }
  protected def goto(stateName: StateName): State = HandlerResult(stateName, stateData)
  protected def stay = goto(stateName)

  def ask(message: Any): Option[Any] = {
    val next = handlers(stateName).orElse(unhandled).apply(Event(message, stateData))
    _nextStateName = next.name
    _nextStateData = next.data
    transitionHandler.applyOrElse((stateName, nextStateName), (_: (S, S)) => ())
    _stateName = _nextStateName
    _stateData = _nextStateData
    next.reply
  }

  /**
   * This extractor is just convenience for matching a (S, S) pair, including a
   * reminder what the new state is.
   */
  object -> {
    def unapply[S](in: (S, S)) = Some(in)
  }
}
