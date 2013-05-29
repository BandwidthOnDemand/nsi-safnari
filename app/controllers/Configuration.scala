package controllers

import play.api.Play.current

object Configuration {
  val Nsa = current.configuration.getString("safnari.requester.nsa").getOrElse(sys.error("safnari.requester.nsa not set"))
}