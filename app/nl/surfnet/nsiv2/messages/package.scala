package nl.surfnet.nsiv2

import java.net.URI

package object messages {
  type RequesterNsa = String
  type ConnectionId = String
  type GlobalReservationId = URI

  implicit class RichString(str: String) {
    def uncapitalize: String = str.take(1).toLowerCase + str.drop(1)
  }
}
