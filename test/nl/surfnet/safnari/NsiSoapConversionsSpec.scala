package nl.surfnet.safnari

import org.w3c.dom.Document
import javax.xml.parsers.DocumentBuilderFactory
import scala.util.Failure
import scala.util.Success

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class NsiSoapConversionsSpec extends helpers.Specification {
  import NsiSoapConversions._

  val DefaultAckHeaders = NsiHeaders(newCorrelationId, "requesterNSA", "providerNSA", None, NsiHeaders.RequesterProtocolVersion, Nil)

  val requestOperationToStringConversion = NsiRequesterMessageToDocument(None)(NsiRequesterOperationToElement).andThen(NsiXmlDocumentConversion.andThen(ByteArrayToString))
  val requestAckToStringConversion = NsiRequesterMessageToDocument(Some(DefaultAckHeaders))(NsiAcknowledgementOperationToElement).andThen(NsiXmlDocumentConversion).andThen(ByteArrayToString)

  val input = """<?xml version="1.0" encoding="UTF-8"?><soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:head="http://schemas.ogf.org/nsi/2013/12/framework/headers" xmlns:type="http://schemas.ogf.org/nsi/2013/12/connection/types">
    <soapenv:Header>
        <head:nsiHeader>
            <protocolVersion>application/vnd.ogf.nsi.cs.v2.provider+soap</protocolVersion>
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
                <p2p:p2ps xmlns:p2p="http://schemas.ogf.org/nsi/2013/12/services/point2point">
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

  "NSI requester operation to string" should {
    "parse reserveFailed operation" in {
      val reserveFailed = """<?xml version="1.0" encoding="UTF-8"?>
        <SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/">
          <SOAP-ENV:Header>
            <ns7:nsiHeader xmlns:ns2="http://schemas.ogf.org/nsi/2013/12/connection/types" xmlns:ns3="urn:oasis:names:tc:SAML:2.0:assertion" xmlns:ns4="http://www.w3.org/2001/04/xmlenc#" xmlns:ns5="http://www.w3.org/2000/09/xmldsig#" xmlns:ns6="http://schemas.ogf.org/nsi/2013/12/framework/types" xmlns:ns7="http://schemas.ogf.org/nsi/2013/12/framework/headers">
              <protocolVersion>application/vnd.ogf.nsi.cs.v2.requester+soap</protocolVersion>
              <correlationId>urn:uuid:fc15890f-3118-442f-8482-da50a303689e</correlationId>
              <requesterNSA>urn:ogf:network:nsa:surfnet-nsi-safnari</requesterNSA>
              <providerNSA>urn:ogf:network:netherlight.net:2013:nsa:bod</providerNSA>
            </ns7:nsiHeader>
          </SOAP-ENV:Header>
          <SOAP-ENV:Body>
            <ns2:reserveFailed xmlns:ns2="http://schemas.ogf.org/nsi/2013/12/connection/types" xmlns:ns3="urn:oasis:names:tc:SAML:2.0:assertion" xmlns:ns4="http://www.w3.org/2001/04/xmlenc#" xmlns:ns5="http://www.w3.org/2000/09/xmldsig#" xmlns:ns6="http://schemas.ogf.org/nsi/2013/12/framework/t ypes" xmlns:ns7="http://schemas.ogf.org/nsi/2013/12/framework/headers">
              <connectionId>3cb4b457-9c50-4285-bd47-f5d93f484dee</connectionId>
              <connectionStates>
                <reservationState>ReserveFailed</reservationState>
                <provisionState>Released</provisionState>
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

      val requesterMessage = requestOperationToStringConversion.invert(reserveFailed)

      requesterMessage must beSuccessfulTry
    }

    "parse dataPlaneStateChange operation" in {
      val message =
        <soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
          <soap:Header>
            <ns6:nsiHeader xmlns:ns8="http://schemas.ogf.org/nsi/2013/12/services/point2point" xmlns:ns7="http://schemas.ogf.org/nsi/2013/12/framework/types" xmlns:ns6="http://schemas.ogf.org/nsi/2013/12/framework/headers" xmlns:ns5="http://schemas.ogf.org/nsi/2013/12/connection/types" xmlns:ns4="http://www.w3.org/2000/09/xmldsig#" xmlns:ns3="http://www.w3.org/2001/04/xmlenc#" xmlns:ns2="urn:oasis:names:tc:SAML:2.0:assertion">
              <protocolVersion>application/vnd.ogf.nsi.cs.v2.provider+soap</protocolVersion>
              <correlationId>urn:uuid:88bbe366-4af7-40bf-8edb-2ad9a980f402</correlationId>
              <requesterNSA>urn:ogf:network:nsa:surfnet-nsi-safnari</requesterNSA>
              <providerNSA>urn:ogf:network:nsa:es.net</providerNSA>
            </ns6:nsiHeader>
          </soap:Header>
          <soap:Body>
            <ns5:dataPlaneStateChange xmlns:ns2="urn:oasis:names:tc:SAML:2.0:assertion" xmlns:ns3="http://www.w3.org/2001/04/xmlenc#" xmlns:ns4="http://www.w3.org/2000/09/xmldsig#" xmlns:ns5="http://schemas.ogf.org/nsi/2013/12/connection/types" xmlns:ns6="http://schemas.ogf.org/nsi/2013/12/framework/headers" xmlns:ns7="http://schemas.ogf.org/nsi/2013/12/framework/types" xmlns:ns8="http://schemas.ogf.org/nsi/2013/12/services/point2point">
              <connectionId>2ac54e1f-3ce6-44af-9356-b0d4e31f3c42</connectionId>
              <notificationId>1</notificationId>
              <timeStamp>2013-09-26T06:19:49.311-07:00</timeStamp>
              <dataPlaneStatus>
                <active>true</active>
                <version>0</version>
                <versionConsistent>true</versionConsistent>
              </dataPlaneStatus>
            </ns5:dataPlaneStateChange>
          </soap:Body>
        </soap:Envelope>.toString

      val requesterMessage = requestOperationToStringConversion.invert(message)

      requesterMessage must beSuccessfulTry
    }

    "parse SOAP fault without serviceException" in {
      val soapFault =
        <soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
          <soap:Body>
            <soap:Fault>
              <faultcode>soap:Server</faultcode>
              <faultstring>Fault occurred while processing.</faultstring>
            </soap:Fault>
          </soap:Body>
        </soap:Envelope>.toString

      val requesterMessage = requestOperationToStringConversion.invert(soapFault)

      requesterMessage must beLike {
        case Failure(ErrorMessageException("SOAP fault without {http://schemas.ogf.org/nsi/2013/12/connection/types}serviceException. Fault string: Fault occurred while processing.")) => ok
      }
    }

    "parse serviceException" in {
      val soapFault =
        <S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/">
          <S:Header>
            <head:nsiHeader xmlns:head="http://schemas.ogf.org/nsi/2013/12/framework/headers">
              <protocolVersion>application/vnd.ogf.nsi.cs.v2.requester+soap</protocolVersion>
              <correlationId>urn:uuid:5c716e15-c17c-481e-885d-c9a5c06e0436</correlationId>
              <requesterNSA>urn:ogf:network:nsa:surfnet-nsi-requester</requesterNSA>
              <providerNSA>urn:ogf:network:nsa:surfnet.nl</providerNSA>
            </head:nsiHeader>
          </S:Header>
          <S:Body>
            <S:Fault xmlns:ns4="http://www.w3.org/2003/05/soap-envelope">
              <faultcode>S:Server</faultcode>
              <faultstring>This operation is not supported by this provider</faultstring>
              <detail>
                <ns5:serviceException xmlns:ns7="http://schemas.ogf.org/nsi/2013/12/framework/headers" xmlns:ns6="http://schemas.ogf.org/nsi/2013/12/framework/types" xmlns:ns4="urn:oasis:names:tc:SAML:2.0:assertion" xmlns:ns3="http://www.w3.org/2000/09/xmldsig#" xmlns:ns2="http://www.w3.org/2001/04/xmlenc#" xmlns:ns5="http://schemas.ogf.org/nsi/2013/12/connection/types">
                  <nsaId>urn:ogf:network:netherlight.net:2013:nsa:bod</nsaId>
                  <errorId>103</errorId>
                  <text>Not Implemented</text>
                </ns5:serviceException>
              </detail>
            </S:Fault>
          </S:Body>
        </S:Envelope>.toString

      val requesterMessage = requestAckToStringConversion.invert(soapFault)

      requesterMessage must beLike {
        case Success(NsiRequesterMessage(headers, ServiceException(exception))) =>
          headers must_== NsiHeaders(
              CorrelationId.fromString("urn:uuid:5c716e15-c17c-481e-885d-c9a5c06e0436").get,
              "urn:ogf:network:nsa:surfnet-nsi-requester",
              "urn:ogf:network:nsa:surfnet.nl",
              None,
              NsiHeaders.RequesterProtocolVersion,
              Nil
          )
          exception.getErrorId() must_== "103"
      }

      requestAckToStringConversion(requesterMessage.get) must beLike {
        case Success(xml) => xml must contain("<S:Fault") and contain("<errorId>103</errorId>")
      }
    }

    "parse serviceException without NSI headers" in {
      val soapFault =
        <S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/">
          <S:Body>
            <S:Fault xmlns:ns4="http://www.w3.org/2003/05/soap-envelope">
              <faultcode>S:Server</faultcode>
              <faultstring>This operation is not supported by this provider</faultstring>
              <detail>
                <ns5:serviceException xmlns:ns7="http://schemas.ogf.org/nsi/2013/12/framework/headers" xmlns:ns6="http://schemas.ogf.org/nsi/2013/12/framework/types" xmlns:ns4="urn:oasis:names:tc:SAML:2.0:assertion" xmlns:ns3="http://www.w3.org/2000/09/xmldsig#" xmlns:ns2="http://www.w3.org/2001/04/xmlenc#" xmlns:ns5="http://schemas.ogf.org/nsi/2013/12/connection/types">
                  <nsaId>urn:ogf:network:netherlight.net:2013:nsa:bod</nsaId>
                  <errorId>103</errorId>
                  <text>Not Implemented</text>
                </ns5:serviceException>
              </detail>
            </S:Fault>
          </S:Body>
        </S:Envelope>.toString

      val requesterMessage = requestAckToStringConversion.invert(soapFault)

      requesterMessage must beLike {
        case Success(NsiRequesterMessage(headers, _)) =>
          headers must_== DefaultAckHeaders
      }
    }
  }

  "DOM to byte array conversion" should {
    "validate and parse byte array" in {
      val Success(dom) = NsiXmlDocumentConversion.invert(input.getBytes("UTF-8"))
      dom must not(beNull)
      dom.getDocumentElement().getLocalName() must beEqualTo("Envelope")

      NsiXmlDocumentConversion(dom).map(new String(_, "UTF-8")).get must beEqualTo(input)
    }
  }
}
