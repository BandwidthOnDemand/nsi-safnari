package nl.surfnet.safnari

import scala.util.control.NoStackTrace

/**
 * Special exception for use with [[scala.util.Try]] when generating a simple error message.
 */
case class ErrorMessageException(message: String) extends RuntimeException(message) with NoStackTrace {
  override def toString() = message
}
