package nl.surfnet.safnari

import org.w3c.dom.Document
import javax.xml.parsers.DocumentBuilderFactory

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class NsiSoapConversionsSpec extends helpers.Specification {
  import NsiSoapConversions._

  val input = """<?xml version="1.0" encoding="UTF-8"?><soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:head="http://schemas.ogf.org/nsi/2013/07/framework/headers" xmlns:type="http://schemas.ogf.org/nsi/2013/07/connection/types">
    <soapenv:Header>
        <head:nsiHeader>
            <protocolVersion>application/vdn.ogf.nsi.cs.v2.provider+soap</protocolVersion>
            <correlationId>urn:uuid:5c716e15-c17c-481e-885d-c9a5c06e0436</correlationId>
            <requesterNSA>urn:ogf:network:nsa:surfnet-nsi-requester</requesterNSA>
            <providerNSA>urn:ogf:network:nsa:surfnet.nl</providerNSA>
            <replyTo>http://localhost:9000/reply</replyTo>
        </head:nsiHeader>
    </soapenv:Header>
    <soapenv:Body>
        <type:reserve>
            <globalReservationId/>
            <description>A NSI reserve test</description>
            <criteria version="0">
                <schedule>
                    <startTime>2013-07-24T16:50:00.000+02:00</startTime>
                    <endTime>2013-07-24T17:00:00.000+02:00</endTime>
                </schedule>
                <p2p:p2ps xmlns:p2p="http://schemas.ogf.org/nsi/2013/07/services/point2point">
                    <capacity>100</capacity>
                    <directionality>Bidirectional</directionality>
                    <sourceSTP>
                        <networkId>urn:ogf:network:stp:surfnet.nl</networkId>
                        <localId>21</localId>
                    </sourceSTP>
                    <destSTP>
                        <networkId>urn:ogf:network:stp:surfnet.nl</networkId>
                        <localId>24</localId>
                    </destSTP>
                </p2p:p2ps>
            </criteria>
        </type:reserve>
    </soapenv:Body>
</soapenv:Envelope>"""

  val reserveFailed = """<?xml version="1.0" encoding="UTF-8"?>
    <SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/">
      <SOAP-ENV:Header>
        <ns7:nsiHeader xmlns:ns2="http://schemas.ogf.org/nsi/2013/07/connection/types"
          xmlns:ns3="urn:oasis:names:tc:SAML:2.0:assertion" xmlns:ns4="http://www.w3.org/2001/04/xmlenc#"
          xmlns:ns5="http://www.w3.org/2000/09/xmldsig#" xmlns:ns6="http://schemas.ogf.org/nsi/2013/07/framework/types"
          xmlns:ns7="http://schemas.ogf.org/nsi/2013/07/framework/headers">
          <protocolVersion>application/vdn.ogf.nsi.cs.v2.requester+soap</protocolVersion>
          <correlationId>urn:uuid:fc15890f-3118-442f-8482-da50a303689e</correlationId>
          <requesterNSA>urn:ogf:network:nsa:surfnet-nsi-safnari</requesterNSA>
          <providerNSA>urn:ogf:network:netherlight.net:2013:nsa:bod</providerNSA>
        </ns7:nsiHeader>
      </SOAP-ENV:Header>
      <SOAP-ENV:Body>
        <ns2:reserveFailed
          xmlns:ns2="http://schemas.ogf.org/nsi/2013/07/connection/types"
          xmlns:ns3="urn:oasis:names:tc:SAML:2.0:assertion" xmlns:ns4="http://www.w3.org/2001/04/xmlenc#"
          xmlns:ns5="http://www.w3.org/2000/09/xmldsig#" xmlns:ns6="http://schemas.ogf.org/nsi/2013/07/framework/t ypes"
          xmlns:ns7="http://schemas.ogf.org/nsi/2013/07/framework/headers">
          <connectionId>3cb4b457-9c50-4285-bd47-f5d93f484dee</connectionId>
          <connectionStates>
            <reservationState>ReserveFailed</reservationState>
            <lifecycleState>Created</lifecycleState>
            <dataPlaneStatus>
              <active>false</active>
              <version>0</version>
              <versionConsistent>true</versionConsistent>
            </dataPlaneStatus>
          </connectionStates>
          <serviceException>
            <nsaId>urn:ogf:network:netherlight.net:2013:nsa:bod</nsaId>
            <connectionId>3cb4b457-9c50-4285-bd47-f5d93f484dee</connectionId>
            <errorId>00200</errorId>
            <text>The VlanID specified in the source parameters overlaps with an existing service.</text>
          </serviceException>
        </ns2:reserveFailed>
      </SOAP-ENV:Body>
    </SOAP-ENV:Envelope>"""

  "NSI requester operation to string" should {
    "parse reserveFailed operation" in {
      val requestOperationToStringConversion = NsiRequesterMessageToDocument(NsiRequesterOperationToJaxbElement).andThen(NsiXmlDocumentConversion.andThen(ByteArrayToString))
      val requesterMessage = requestOperationToStringConversion.invert(reserveFailed)

      requesterMessage must beRight
    }
  }

  "DOM to byte array conversion" should {
    "validate and parse byte array" in {
      val Right(dom) = NsiXmlDocumentConversion.invert(input.getBytes("UTF-8"))
      dom must not(beNull)
      dom.getDocumentElement().getLocalName() must beEqualTo("Envelope")

      NsiXmlDocumentConversion(dom).right.map(new String(_, "UTF-8")).right.get must beEqualTo(input)
    }
  }
}
