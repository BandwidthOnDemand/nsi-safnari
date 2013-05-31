package nl.surfnet.safnari

import java.io.{ ByteArrayInputStream, ByteArrayOutputStream }
import java.net.URI
import javax.xml.XMLConstants
import javax.xml.bind.JAXBContext
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.soap.{ MessageFactory, MimeHeaders, SOAPElement, SOAPFactory, SOAPMessage }
import javax.xml.transform.Source
import javax.xml.transform.stream.StreamSource
import javax.xml.validation.SchemaFactory
import org.ogf.schemas.nsi._2013._04.connection.types._
import org.ogf.schemas.nsi._2013._04.framework.headers.CommonHeaderType
import org.ogf.schemas.nsi._2013._04.framework.types.ServiceExceptionType
import org.w3c.dom.{ Document, Element, Node }
import scala.collection.JavaConverters._
import scala.reflect.ClassTag
import javax.xml.bind.JAXBElement

object NsiSoapConversions {
  implicit val SoapMessageToByteArray = Conversion.build[SOAPMessage, Array[Byte]] { soapMessage =>
    tryEither {
      new ByteArrayOutputStream().tap(soapMessage.writeTo).toByteArray
    }
  } { bytes =>
    tryEither {
      MessageFactory.newInstance().createMessage(new MimeHeaders, new ByteArrayInputStream(bytes))
    }
  }

  implicit val SoapMessageToString = Conversion.build[SOAPMessage, String] { soapMessage =>
    tryEither {
      new ByteArrayOutputStream().tap(soapMessage.writeTo).toString("UTF-8")
    }
  } { string =>
    tryEither {
      MessageFactory.newInstance().createMessage(new MimeHeaders, new ByteArrayInputStream(string.getBytes("UTF-8")))
    }
  }

  private val factory = new org.ogf.schemas.nsi._2013._04.connection.types.ObjectFactory()

  implicit val NsiAcknowledgementOperationToSoapMessage: Conversion[NsiAcknowledgement, SOAPMessage] = SoapConversion {
    messageFactories(Map(
      "acknowledgment" -> NsiMessageFactory[GenericAcknowledgmentType, NsiAcknowledgement]((headers, _) => GenericAck(headers)),
      "reserveResponse" -> NsiMessageFactory[ReserveResponseType, NsiAcknowledgement]((headers, body) => ReserveResponse(headers, body.getConnectionId())),
      "serviceException" -> NsiMessageFactory[ServiceExceptionType, NsiAcknowledgement]((headers, body) => ServiceException(headers, body)),
      "querySummarySyncConfirmed" -> NsiMessageFactory[QuerySummaryConfirmedType, NsiAcknowledgement]((headers, body) => QuerySummarySyncConfirmed(headers, body.getReservation().asScala.to[Vector]))))
  } {
    case GenericAck(_)                              => factory.createAcknowledgment(new GenericAcknowledgmentType())
    case ReserveResponse(_, connectionId)           => factory.createReserveResponse(new ReserveResponseType().withConnectionId(connectionId))
    case ServiceException(_, exception)             => factory.createServiceException(exception)
    case QuerySummarySyncConfirmed(_, reservations) => factory.createQuerySummarySyncConfirmed(new QuerySummaryConfirmedType().withReservation(reservations.asJava))
  }

  implicit val NsiProviderOperationToSoapMessage: Conversion[NsiProviderOperation, SOAPMessage] = SoapConversion {
    messageFactories(Map(
      "reserve" -> NsiMessageFactory[ReserveType, NsiProviderOperation](Reserve),
      "reserveCommit" -> NsiMessageFactory[GenericRequestType, NsiProviderOperation]((correlationId, body) => ReserveCommit(correlationId, body.getConnectionId())),
      "reserveAbort" -> NsiMessageFactory[GenericRequestType, NsiProviderOperation]((correlationId, body) => ReserveAbort(correlationId, body.getConnectionId())),
      "provision" -> NsiMessageFactory[GenericRequestType, NsiProviderOperation]((correlationId, body) => Provision(correlationId, body.getConnectionId())),
      "release" -> NsiMessageFactory[GenericRequestType, NsiProviderOperation]((correlationId, body) => Release(correlationId, body.getConnectionId())),
      "terminate" -> NsiMessageFactory[GenericRequestType, NsiProviderOperation]((correlationId, body) => Terminate(correlationId, body.getConnectionId())),
      "querySummary" -> NsiMessageFactory[QueryType, NsiProviderOperation]((correlationId, body) => QuerySummary(correlationId, body.getConnectionId().asScala)),
      "querySummarySync" -> NsiMessageFactory[QueryType, NsiProviderOperation]((correlationId, body) => QuerySummarySync(correlationId, body.getConnectionId().asScala))))
  } {
    case Reserve(_, body)                   => factory.createReserve(body)
    case ReserveCommit(_, connectionId)     => factory.createReserveCommit(new GenericRequestType().withConnectionId(connectionId))
    case ReserveAbort(_, connectionId)      => factory.createReserveAbort(new GenericRequestType().withConnectionId(connectionId))
    case Provision(_, connectionId)         => factory.createProvision(new GenericRequestType().withConnectionId(connectionId))
    case Release(_, connectionId)           => factory.createRelease(new GenericRequestType().withConnectionId(connectionId))
    case Terminate(_, connectionId)         => factory.createTerminate(new GenericRequestType().withConnectionId(connectionId))
    case QuerySummary(_, connectionIds)     => factory.createQuerySummary(new QueryType().withConnectionId(connectionIds.asJava))
    case QuerySummarySync(_, connectionIds) => factory.createQuerySummarySync(new QueryType().withConnectionId(connectionIds.asJava))
    case QueryRecursive(_, connectionIds)   => factory.createQueryRecursive(new QueryType().withConnectionId(connectionIds.asJava))
  }

  implicit val NsiRequesterOperationToSoapMessage: Conversion[NsiRequesterOperation, SOAPMessage] = SoapConversion {
    messageFactories(Map(
      // FIXME this list seems to be incomplete?
      "reserveConfirmed" -> NsiMessageFactory[ReserveConfirmedType, NsiRequesterOperation]((correlationId, body) => ReserveConfirmed(correlationId, body.getConnectionId(), body.getCriteria().asScala.head)),
      "reserveCommitConfirmed" -> NsiMessageFactory[GenericConfirmedType, NsiRequesterOperation]((correlationId, body) => ReserveCommitConfirmed(correlationId, body.getConnectionId)),
      "provisionConfirmed" -> NsiMessageFactory[GenericConfirmedType, NsiRequesterOperation]((correlationId, body) => ProvisionConfirmed(correlationId, body.getConnectionId)),
      "releaseConfirmed" -> NsiMessageFactory[GenericConfirmedType, NsiRequesterOperation]((correlationId, body) => ReleaseConfirmed(correlationId, body.getConnectionId)),
      "terminateConfirmed" -> NsiMessageFactory[GenericConfirmedType, NsiRequesterOperation]((correlationId, body) => TerminateConfirmed(correlationId, body.getConnectionId)),
      "dataPlaneStateChange" -> NsiMessageFactory[DataPlaneStateChangeRequestType, NsiRequesterOperation]((correlationId, body) => DataPlaneStateChange(correlationId, body.getConnectionId(), body.getDataPlaneStatus(), body.getTimeStamp()))))
  } {
    case ReserveConfirmed(_, connectionId, criteria)              => factory.createReserveConfirmed(new ReserveConfirmedType().withConnectionId(connectionId).withCriteria(criteria))
    case ReserveFailed(_, failure)                                => factory.createReserveFailed(failure)
    case ReserveCommitConfirmed(_, connectionId)                  => factory.createReserveCommitConfirmed(new GenericConfirmedType().withConnectionId(connectionId))
    case ReserveCommitFailed(_, failure)                          => factory.createReserveCommitFailed(failure)
    case ReserveAbortConfirmed(_, connectionId)                   => factory.createReserveAbortConfirmed(new GenericConfirmedType().withConnectionId(connectionId))
    case ReserveTimeout(_, timeout)                               => factory.createReserveTimeout(timeout)
    case ProvisionConfirmed(_, connectionId)                      => factory.createProvisionConfirmed(new GenericConfirmedType().withConnectionId(connectionId))
    case ReleaseConfirmed(_, connectionId)                        => factory.createReleaseConfirmed(new GenericConfirmedType().withConnectionId(connectionId))
    case TerminateConfirmed(_, connectionId)                      => factory.createTerminateConfirmed(new GenericConfirmedType().withConnectionId(connectionId))
    case QuerySummaryConfirmed(_, reservations)                   => factory.createQuerySummaryConfirmed(new QuerySummaryConfirmedType().withReservation(reservations.asJava))
    case QuerySummaryFailed(_, failed)                            => factory.createQuerySummaryFailed(failed)
    case QueryRecursiveConfirmed(_, reservations)                 => factory.createQueryRecursiveConfirmed(new QueryRecursiveConfirmedType().withReservation(reservations.asJava))
    case QueryRecursiveFailed(_, failed)                          => factory.createQueryRecursiveFailed(failed)
    case DataPlaneStateChange(_, connectionId, status, timeStamp) => factory.createDataPlaneStateChange(new DataPlaneStateChangeRequestType().withConnectionId(connectionId).withDataPlaneStatus(status).withTimeStamp(timeStamp))
    case ErrorEvent(_, error)                                     => factory.createErrorEvent(error)
    case MessageDeliveryTimeout(correlationId, timeStamp)         => factory.createMessageDeliveryTimeout(new MessageDeliveryTimeoutRequestType().withCorrelationId(correlationId.toString).withTimeStamp(timeStamp))
  }

  private val headersFactory = new org.ogf.schemas.nsi._2013._04.framework.headers.ObjectFactory()

  private implicit val NsiHeadersToElement = Conversion.build[NsiHeaders, Element] { headers =>
    val header = new CommonHeaderType()
      .withCorrelationId(headers.correlationId.toString)
      .withReplyTo(headers.replyTo.map(_.toASCIIString()).orNull)
      .withProtocolVersion(headers.protocolVersion.toASCIIString())
      .withProviderNSA(headers.providerNSA)
      .withRequesterNSA(headers.requesterNSA)

    marshal(headersFactory.createNsiHeader(header)).right.map(_.getDocumentElement())
  } { element =>
    for {
      header <- unmarshal(element, classOf[CommonHeaderType]).right
      correlationId <- CorrelationId.fromString(header.getCorrelationId()).toRight(s"invalid correlation id ${header.getCorrelationId()}").right
      replyTo <- tryEither(Option(header.getReplyTo()).map(URI.create)).right
      protocolVersion <- tryEither(URI.create(header.getProtocolVersion())).right
    } yield {
      NsiHeaders(correlationId, header.getRequesterNSA(), header.getProviderNSA(), replyTo, protocolVersion)
    }
  }

  private val SchemaLocations = Seq("wsdl/2.0/ogf_nsi_framework_headers_v2_0.xsd", "wsdl/2.0/ogf_nsi_connection_types_v2_0.xsd")
  private val SchemaPackages = Seq("org.ogf.schemas.nsi._2013._04.framework.headers", "org.ogf.schemas.nsi._2013._04.connection.types")

  private val nsiSchema = {
    val schemaSources = SchemaLocations.map(location => new StreamSource(classpathResourceUri(location).toASCIIString()))

    val factory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)
    factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true)
    factory.newSchema(schemaSources.toArray[Source])
  }
  private val jaxbContext = JAXBContext.newInstance(SchemaPackages.mkString(":"))

  private def unmarshal[T](node: Node, klass: Class[T]): Either[String, T] = tryEither {
    /* To allow whitespace in between elements set the following event handler on the unmarshaller:
     new ValidationEventHandler {
       def handleEvent(event: ValidationEvent) = event.getMessage().contains("cvc-complex-type.2.3") && event.getMessage().contains("cannot have character [children], because the type's content type is element-only")
     }
     */
    val unmarshaller = jaxbContext.createUnmarshaller().tap(_.setSchema(nsiSchema))
    unmarshaller.unmarshal(node, klass).getValue()
  }

  private def marshal(jaxbElement: JAXBElement[_]): Either[String, Document] = tryEither {
    val newDocument = DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument()
    val marshaller = jaxbContext.createMarshaller().tap(_.setSchema(nsiSchema))
    newDocument.tap(document => marshaller.marshal(jaxbElement, document))
  }

  private trait NsiMessageFactory[T] {
    type JaxbMessage
    def klass: Class[JaxbMessage]
    def apply(headers: NsiHeaders, body: JaxbMessage): T
  }
  private object NsiMessageFactory {
    def apply[M, T](f: (NsiHeaders, M) => T)(implicit manifest: ClassTag[M]) = new NsiMessageFactory[T] {
      override type JaxbMessage = M
      override def klass = manifest.runtimeClass.asInstanceOf[Class[M]]
      override def apply(headers: NsiHeaders, body: M): T = f(headers, body)
    }
  }

  private def messageFactories[T <: NsiMessage](factories: Map[String, NsiMessageFactory[T]])(bodyNode: Element): Either[String, NsiMessageFactory[T]] =
    factories.get(bodyNode.getLocalName()).toRight(s"unknown body element type '${bodyNode.getLocalName}'")

  private val NsiFrameworkHeaderNamespace = "http://schemas.ogf.org/nsi/2013/04/framework/headers"
  private val NsiConnectionTypesNamespace = "http://schemas.ogf.org/nsi/2013/04/connection/types"

  private def SoapConversion[T <: NsiMessage](elementToFactory: Element => Either[String, NsiMessageFactory[T]])(messageToJaxb: T => JAXBElement[_]): Conversion[T, SOAPMessage] = Conversion.build[T, SOAPMessage] { message =>
    def buildSoapMessage(headerElement: Element, bodyDocument: Document) = tryEither {
      val soap = MessageFactory.newInstance().createMessage()
      val header = SOAPFactory.newInstance().createElement(headerElement)
      soap.getSOAPHeader().addChildElement(header)
      soap.getSOAPBody().addDocument(bodyDocument)
      soap.saveChanges()
      soap
    }

    for {
      headerElement <- Conversion[NsiHeaders, Element].apply(message.headers).right
      bodyDocument <- marshal(messageToJaxb(message)).right
      soap <- buildSoapMessage(headerElement, bodyDocument).right
    } yield {
      soap
    }
  } { soapMessage =>
    for {
      headerNode <- onlyChildElementWithNamespace(NsiFrameworkHeaderNamespace, soapMessage.getSOAPHeader()).right
      headers <- Conversion[NsiHeaders, Element].invert(headerNode).right
      bodyNode <- onlyChildElementWithNamespace(NsiConnectionTypesNamespace, soapMessage.getSOAPBody()).right
      messageFactory <- elementToFactory(bodyNode).right
      body <- unmarshal(bodyNode, messageFactory.klass).right
    } yield {
      messageFactory(headers, body)
    }
  }

  private def onlyChildElementWithNamespace(namespaceUri: String, elem: SOAPElement): Either[String, Element] =
    elem.getChildElements().asScala.collect {
      case e: Element if e.getNamespaceURI() == namespaceUri => e
    }.toList match {
      case Nil      => Left(s"missing NSI element in '${elem.getLocalName}', expected exactly one")
      case e :: Nil => Right(e)
      case _        => Left(s"multiple NSI elements in '${elem.getLocalName}', expected exactly one")
    }
}
