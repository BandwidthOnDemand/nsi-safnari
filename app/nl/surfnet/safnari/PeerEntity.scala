package nl.surfnet.safnari

/**
 * Holds information about a peered NSA.  Is used to generate the <peersWith>
 * element in the nsa-discovery XML file.
 */
case class PeerEntity(id: Option[String], dn: Option[String])
