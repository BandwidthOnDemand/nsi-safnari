package nl.surfnet.nsiv2.utils

import scala.util.control.NoStackTrace

case class ErrorMessage(message: String) extends RuntimeException(message) with NoStackTrace {
  override def toString = message
}
