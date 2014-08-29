package nl.surfnet.safnari

/**
 * Holds information about a peered NSA.  Is used to generate the <peersWith>
 * element in the nsa-discovery XML file.
 *
 * Created by jmacauley on 2014-08-28.
 */
class PeerEntity(idn : Option[String], dsn : Option[String]) {
  val id : Option[String] = idn
  val dn : Option[String] = dsn
}