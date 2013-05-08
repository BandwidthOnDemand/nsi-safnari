package nl.surfnet.safnari

import org.w3c.dom.Document
import org.ogf.schemas.nsi._2013._04.connection.types._
import org.ogf.schemas.nsi._2013._04.framework.types.ServiceExceptionType
import scala.collection.JavaConverters._
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.transform.stream.StreamSource
import javax.xml.validation.SchemaFactory
import javax.xml.bind.JAXBContext
import javax.xml.XMLConstants
import javax.xml.transform.Source

trait ToXmlDocument[-A] {
  def asDocument(a: A): Document
}
object ToXmlDocument {
  def apply[A](implicit toXmlDocument: ToXmlDocument[A]) = toXmlDocument

  private def newDocument = DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument()

  private val SchemaLocations = Seq("wsdl/2.0/ogf_nsi_framework_headers_v2_0.xsd", "wsdl/2.0/ogf_nsi_connection_types_v2_0.xsd")
  private val SchemaPackages = Seq("org.ogf.schemas.nsi._2013._04.framework.headers", "org.ogf.schemas.nsi._2013._04.connection.types")

  private val nsiSchema = {
    val schemaSources = SchemaLocations.map(location => new StreamSource(classpathResourceUri(location).toASCIIString()))

    val factory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)
    factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true)
    factory.newSchema(schemaSources.toArray[Source])
  }
  private val jaxbContext = JAXBContext.newInstance(SchemaPackages.mkString(":"))

  private def marshaller = jaxbContext.createMarshaller().tap(_.setSchema(nsiSchema))
  def unmarshaller = jaxbContext.createUnmarshaller().tap(_.setSchema(nsiSchema))

  /* To allow whitespace in between elements set the following event handler on the unmarshaller:
     new ValidationEventHandler {
       def handleEvent(event: ValidationEvent) = event.getMessage().contains("cvc-complex-type.2.3") && event.getMessage().contains("cannot have character [children], because the type's content type is element-only")
     }
   */

  def marshal(jaxbElement: AnyRef): Document = newDocument.tap(document => marshaller.marshal(jaxbElement, document))

  implicit val NsiHeadersToXmlDocument = new ToXmlDocument[NsiHeaders] {
    private val factory = new org.ogf.schemas.nsi._2013._04.framework.headers.ObjectFactory()

    def asDocument(a: NsiHeaders) = {
      val header = factory.createCommonHeaderType()
      header.setCorrelationId(a.correlationId.toString)
      header.setReplyTo(a.replyTo.map(_.toASCIIString()).orNull)
      header.setProtocolVersion(a.protocolVersion.toASCIIString())
      header.setProviderNSA(a.providerNSA)
      header.setRequesterNSA(a.requesterNSA)

      marshal(factory.createNsiHeader(header))
    }
  }

  implicit val NsiAcknowledgementToXmlDocument = new ToXmlDocument[NsiAcknowledgement] {
    private val factory = new ObjectFactory()

    def asDocument(a: NsiAcknowledgement) = {
      marshal(a match {
        case GenericAck(_) =>
          factory.createAcknowledgment(new GenericAcknowledgmentType())
        case ReserveResponse(_, connectionId) =>
          factory.createReserveResponse(new ReserveResponseType().withConnectionId(connectionId))
        case ServiceException(_, text) =>
          factory.createServiceException(
            new ServiceExceptionType()
              .withNsaId("MYNSAID") // TODO
              .withErrorId("UNKNOWN") // TODO
              .withText(text)
              .withVariables(null)) // TODO
        case QuerySummarySyncConfirmed(_, results) =>
          factory.createQuerySummarySyncConfirmed(new QuerySummaryConfirmedType().withReservation(results.asJava))
      })
    }
  }

  implicit val NsiRequesterOperationToXmlDocument = new ToXmlDocument[NsiRequesterOperation] {
    private val factory = new ObjectFactory()

    def asDocument(a: NsiRequesterOperation) = marshal(a match {
      case ReserveConfirmed(_, connectionId, criteria) =>
        val confirmed = factory.createReserveConfirmedType().withConnectionId(connectionId).withCriteria(criteria)
        factory.createReserveConfirmed(confirmed)
      case ReserveFailed(_, connectionId) =>
        val genericFailed = factory.createGenericFailedType()
          .withConnectionId(connectionId)
          .withConnectionStates(factory.createConnectionStatesType()
            .withDataPlaneStatus(factory.createDataPlaneStatusType()
              .withActive(false)
              .withVersion(0)
              .withVersionConsistent(true))
            .withLifecycleState(factory.createLifecycleStateType().withState(LifecycleStateEnumType.TERMINATED))
            .withProvisionState(factory.createProvisionStateType().withState(ProvisionStateEnumType.RELEASED))
            .withReservationState(factory.createReservationStateType().withState(ReservationStateEnumType.RESERVE_FAILED)))
          .withServiceException(new ServiceExceptionType()
            .withErrorId("0600")
            .withNsaId("urn:ogf:surfnet.nl")
            .withText("Creating reservation is not supported yet"))
        factory.createReserveFailed(genericFailed)
      case ReserveCommitConfirmed(_, connectionId) =>
        val confirmed = factory.createGenericConfirmedType().withConnectionId(connectionId)
        factory.createReserveCommitConfirmed(confirmed)
      case QuerySummaryConfirmed(_, reservations) =>
        factory.createQuerySummaryConfirmed(new QuerySummaryConfirmedType().withReservation(reservations.asJava))
      case _ => ???
    })
  }

  implicit val NsiProviderOperationToXmlDocument = new ToXmlDocument[NsiProviderOperation] {
    private val factory = new ObjectFactory()

    def asDocument(a: NsiProviderOperation) = marshal(a match {
      case Reserve(_, body)               => factory.createReserve(body)
      case ReserveCommit(_, connectionId) => factory.createReserveCommit(new GenericRequestType().withConnectionId(connectionId))
      case ReserveAbort(_, connectionId)  => factory.createReserveAbort(new GenericRequestType().withConnectionId(connectionId))
      case _                              => ???
    })
  }
}
