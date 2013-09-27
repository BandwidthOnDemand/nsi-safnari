package nl.surfnet.safnari

import play.api.Logger

/**
 * Simplified re-implementation of Akka's finite state machine [[akka.actor.FSM]]
 * DSL, without the dependencies on actors.
 */
abstract class FiniteStateMachine[S, D, I, O](initialStateName: S, initialStateData: D) {

  /**
   * Process the given message. Returns `None` if the FSM did not handle the
   * message. Otherwise the replies (if any) are returned.
   */
  def process(message: I): Option[Seq[O]] = {
    val nextState = _handlers(stateName).orElse(_unhandled).lift.apply(Event(message, _stateData))
    nextState map { nextState =>
      _nextStateName = nextState.name
      _nextStateData = nextState.data
      Logger.debug(s"state change from ${_stateName} to ${_nextStateName}")
      val output = _transitionHandler.applyOrElse((_stateName, _nextStateName), (_: (S, S)) => Vector.empty)
      _stateName = _nextStateName
      _stateData = _nextStateData
      output
    }
  }

  /**
   * This captures all of the managed state of the state machine: the state
   * name, the state data, and replies accumulated while processing the last
   * message.
   */
  protected[this] case class State(name: S, data: D) {
    def using(nextData: D) = copy(data = nextData)
  }
  protected[this] case class Event(message: I, data: D)

  protected[this] def stateName = _stateName
  protected[this] def stateData = _stateData

  protected[this] def nextStateName = _nextStateName
  protected[this] def nextStateData = _nextStateData

  protected[this]type EventHandler = PartialFunction[Event, State]
  protected[this]type TransitionHandler = PartialFunction[(S, S), Seq[O]]

  protected[this] def when(stateName: S)(handler: EventHandler) {
    require(!_handlers.contains(stateName), s"handler for state $stateName is already defined")
    _handlers += stateName -> handler
  }
  protected[this] def whenUnhandled(handler: EventHandler) {
    _unhandled = handler
  }
  protected[this] def onTransition(handler: TransitionHandler) {
    _transitionHandler = handler
  }

  protected[this] def goto(stateName: S): State = {
    require(_handlers contains stateName, s"cannot goto $stateName: state does not exist")
    State(stateName, stateData)
  }
  protected[this] def stay = goto(stateName)

  /**
   * This extractor is just convenience for matching a (S, S) pair, including a
   * reminder what the new state is.
   */
  protected[this] object -> {
    def unapply[S](in: (S, S)) = Some(in)
  }

  private var _stateName: S = initialStateName
  private var _stateData: D = initialStateData
  private var _nextStateName: S = stateName
  private var _nextStateData: D = stateData

  private var _handlers: Map[S, EventHandler] = Map.empty
  private var _unhandled: EventHandler = PartialFunction.empty
  private var _transitionHandler: TransitionHandler = { case _ => Vector.empty }
}
