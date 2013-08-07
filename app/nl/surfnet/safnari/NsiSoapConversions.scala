package nl.surfnet.safnari

import java.io.{ ByteArrayInputStream, ByteArrayOutputStream }
import java.net.URI
import javax.xml.XMLConstants
import javax.xml.bind.JAXBContext
import javax.xml.bind.JAXBElement
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.transform.Source
import javax.xml.transform.TransformerFactory
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult
import javax.xml.transform.stream.StreamSource
import javax.xml.validation.SchemaFactory
import org.ogf.schemas.nsi._2013._07.connection.types._
import org.ogf.schemas.nsi._2013._07.framework.headers.CommonHeaderType
import org.ogf.schemas.nsi._2013._07.framework.types.ServiceExceptionType
import org.w3c.dom.{ Document, Element, Node }
import org.xml.sax.helpers.DefaultHandler
import org.xml.sax.SAXParseException
import scala.collection.JavaConverters._
import scala.reflect.ClassTag

object NsiSoapConversions {
  implicit val ByteArrayToString = Conversion.build[Array[Byte], String] { bytes =>
    tryEither(new String(bytes, "UTF-8"))
  } { string =>
    tryEither(string.getBytes("UTF-8"))
  }

  private val typesFactory = new org.ogf.schemas.nsi._2013._07.connection.types.ObjectFactory()
  private val headersFactory = new org.ogf.schemas.nsi._2013._07.framework.headers.ObjectFactory()
  private val pointToPointServiceFactory = new org.ogf.schemas.nsi._2013._07.services.point2point.ObjectFactory()
  private val SchemaPackages = Seq(typesFactory, headersFactory, pointToPointServiceFactory).map(_.getClass().getPackage().getName())

  implicit val NsiAcknowledgementOperationToDocument: Conversion[NsiAcknowledgement, Document] = NsiMessageToDocumentConversion {
    messageFactories(Map(
      "acknowledgment" -> NsiMessageFactory[GenericAcknowledgmentType, NsiAcknowledgement]((headers, _) => GenericAck(headers)),
      "reserveResponse" -> NsiMessageFactory[ReserveResponseType, NsiAcknowledgement]((headers, body) => ReserveResponse(headers, body.getConnectionId())),
      "serviceException" -> NsiMessageFactory[ServiceExceptionType, NsiAcknowledgement]((headers, body) => ServiceException(headers, body)),
      "querySummarySyncConfirmed" -> NsiMessageFactory[QuerySummaryConfirmedType, NsiAcknowledgement]((headers, body) => QuerySummarySyncConfirmed(headers, body.getReservation().asScala.to[Vector]))))
  } {
    case GenericAck(_)                              => typesFactory.createAcknowledgment(new GenericAcknowledgmentType())
    case ReserveResponse(_, connectionId)           => typesFactory.createReserveResponse(new ReserveResponseType().withConnectionId(connectionId))
    case ServiceException(_, exception)             => typesFactory.createServiceException(exception)
    case QuerySummarySyncConfirmed(_, reservations) => typesFactory.createQuerySummarySyncConfirmed(new QuerySummaryConfirmedType().withReservation(reservations.asJava))
  }

  implicit val NsiProviderOperationToDocument: Conversion[NsiProviderOperation, Document] = NsiMessageToDocumentConversion {
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
    case Reserve(_, body)                   => typesFactory.createReserve(body)
    case ReserveCommit(_, connectionId)     => typesFactory.createReserveCommit(new GenericRequestType().withConnectionId(connectionId))
    case ReserveAbort(_, connectionId)      => typesFactory.createReserveAbort(new GenericRequestType().withConnectionId(connectionId))
    case Provision(_, connectionId)         => typesFactory.createProvision(new GenericRequestType().withConnectionId(connectionId))
    case Release(_, connectionId)           => typesFactory.createRelease(new GenericRequestType().withConnectionId(connectionId))
    case Terminate(_, connectionId)         => typesFactory.createTerminate(new GenericRequestType().withConnectionId(connectionId))
    case QuerySummary(_, connectionIds)     => typesFactory.createQuerySummary(new QueryType().withConnectionId(connectionIds.asJava))
    case QuerySummarySync(_, connectionIds) => typesFactory.createQuerySummarySync(new QueryType().withConnectionId(connectionIds.asJava))
    case QueryRecursive(_, connectionIds)   => typesFactory.createQueryRecursive(new QueryType().withConnectionId(connectionIds.asJava))
  }

  implicit val NsiRequesterOperationToDocument: Conversion[NsiRequesterOperation, Document] = NsiMessageToDocumentConversion {
    messageFactories(Map(
      // FIXME this list seems to be incomplete?
      "reserveConfirmed" -> NsiMessageFactory[ReserveConfirmedType, NsiRequesterOperation]((headers, body) => ReserveConfirmed(headers, body.getConnectionId(), body.getCriteria())),
      "reserveCommitConfirmed" -> NsiMessageFactory[GenericConfirmedType, NsiRequesterOperation]((headers, body) => ReserveCommitConfirmed(headers, body.getConnectionId)),
      "reserveTimeout" -> NsiMessageFactory[ReserveTimeoutRequestType, NsiRequesterOperation]((headers, body) => ReserveTimeout(headers, body)),
      "provisionConfirmed" -> NsiMessageFactory[GenericConfirmedType, NsiRequesterOperation]((headers, body) => ProvisionConfirmed(headers, body.getConnectionId)),
      "releaseConfirmed" -> NsiMessageFactory[GenericConfirmedType, NsiRequesterOperation]((headers, body) => ReleaseConfirmed(headers, body.getConnectionId)),
      "terminateConfirmed" -> NsiMessageFactory[GenericConfirmedType, NsiRequesterOperation]((headers, body) => TerminateConfirmed(headers, body.getConnectionId)),
      "dataPlaneStateChange" -> NsiMessageFactory[DataPlaneStateChangeRequestType, NsiRequesterOperation]((headers, body) => DataPlaneStateChange(headers, body.getConnectionId(), body.getDataPlaneStatus(), body.getTimeStamp()))))
  } {
    case ReserveConfirmed(_, connectionId, criteria)              => typesFactory.createReserveConfirmed(new ReserveConfirmedType().withConnectionId(connectionId).withCriteria(criteria))
    case ReserveFailed(_, failure)                                => typesFactory.createReserveFailed(failure)
    case ReserveCommitConfirmed(_, connectionId)                  => typesFactory.createReserveCommitConfirmed(new GenericConfirmedType().withConnectionId(connectionId))
    case ReserveCommitFailed(_, failure)                          => typesFactory.createReserveCommitFailed(failure)
    case ReserveAbortConfirmed(_, connectionId)                   => typesFactory.createReserveAbortConfirmed(new GenericConfirmedType().withConnectionId(connectionId))
    case ReserveTimeout(_, timeout)                               => typesFactory.createReserveTimeout(timeout)
    case ProvisionConfirmed(_, connectionId)                      => typesFactory.createProvisionConfirmed(new GenericConfirmedType().withConnectionId(connectionId))
    case ReleaseConfirmed(_, connectionId)                        => typesFactory.createReleaseConfirmed(new GenericConfirmedType().withConnectionId(connectionId))
    case TerminateConfirmed(_, connectionId)                      => typesFactory.createTerminateConfirmed(new GenericConfirmedType().withConnectionId(connectionId))
    case QuerySummaryConfirmed(_, reservations)                   => typesFactory.createQuerySummaryConfirmed(new QuerySummaryConfirmedType().withReservation(reservations.asJava))
    case QuerySummaryFailed(_, failed)                            => typesFactory.createQuerySummaryFailed(failed)
    case QueryRecursiveConfirmed(_, reservations)                 => typesFactory.createQueryRecursiveConfirmed(new QueryRecursiveConfirmedType().withReservation(reservations.asJava))
    case QueryRecursiveFailed(_, failed)                          => typesFactory.createQueryRecursiveFailed(failed)
    case DataPlaneStateChange(_, connectionId, status, timeStamp) => typesFactory.createDataPlaneStateChange(new DataPlaneStateChangeRequestType().withConnectionId(connectionId).withDataPlaneStatus(status).withTimeStamp(timeStamp))
    case ErrorEvent(_, error)                                     => typesFactory.createErrorEvent(error)
    case MessageDeliveryTimeout(correlationId, timeStamp)         => typesFactory.createMessageDeliveryTimeout(new MessageDeliveryTimeoutRequestType().withCorrelationId(correlationId.toString).withTimeStamp(timeStamp))
  }

  private implicit val NsiHeadersToCommonHeaderType = Conversion.build[NsiHeaders, CommonHeaderType] { headers =>
    Right(new CommonHeaderType()
      .withCorrelationId(headers.correlationId.toString)
      .withReplyTo(headers.replyTo.map(_.toASCIIString()).orNull)
      .withProtocolVersion(headers.protocolVersion.toASCIIString())
      .withProviderNSA(headers.providerNSA)
      .withRequesterNSA(headers.requesterNSA))
  } { header =>
    for {
      correlationId <- CorrelationId.fromString(header.getCorrelationId()).toRight(s"invalid correlation id ${header.getCorrelationId()}").right
      replyTo <- tryEither(Option(header.getReplyTo()).map(URI.create)).right
      protocolVersion <- tryEither(URI.create(header.getProtocolVersion())).right
    } yield {
      NsiHeaders(correlationId, header.getRequesterNSA(), header.getProviderNSA(), replyTo, protocolVersion)
    }
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

  private val SoapNamespaceUri = "http://schemas.xmlsoap.org/soap/envelope/"
  private val NsiFrameworkHeaderNamespace = "http://schemas.ogf.org/nsi/2013/07/framework/headers"
  private val NsiConnectionTypesNamespace = "http://schemas.ogf.org/nsi/2013/07/connection/types"

  val NsiXmlDocumentConversion = XmlDocumentConversion("wsdl/soap/soap-envelope-1.1.xsd", "wsdl/2.0/ogf_nsi_framework_headers_v2_0.xsd", "wsdl/2.0/ogf_nsi_connection_types_v2_0.xsd")

  private def XmlDocumentConversion(schemaLocations: String*): Conversion[Document, Array[Byte]] = {
    val schemaSources = schemaLocations.map(location => new StreamSource(classpathResourceUri(location).toASCIIString()))
    val schemaFactory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)
    schemaFactory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true)
    val schema = schemaFactory.newSchema(schemaSources.toArray[Source])
    val errorHandler = new DefaultHandler() {
      override def error(e: SAXParseException) = throw e
      override def fatalError(e: SAXParseException) = throw e
    }

    Conversion.build[Document, Array[Byte]] { document =>
      tryEither {
        val domSource = new DOMSource(document)

        val validator = schema.newValidator()
        validator.setErrorHandler(errorHandler)
        validator.validate(domSource)

        val transformer = TransformerFactory.newInstance().newTransformer()

        val baos = new ByteArrayOutputStream()
        transformer.transform(domSource, new StreamResult(baos))
        baos.toByteArray()
      }
    } { bytes =>
      tryEither {
        val dbf = DocumentBuilderFactory.newInstance()
        dbf.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true)
        dbf.setNamespaceAware(true)
        dbf.setIgnoringComments(true)
        dbf.setIgnoringElementContentWhitespace(true)
        dbf.setSchema(schema)

        val db = dbf.newDocumentBuilder()
        db.setErrorHandler(errorHandler)
        db.parse(new ByteArrayInputStream(bytes))
      }
    }
  }

  val jaxbContext = JAXBContext.newInstance(SchemaPackages.mkString(":"))

  def NsiMessageToDocumentConversion[T <: NsiMessage](elementToFactory: Element => Either[String, NsiMessageFactory[T]])(messageToJaxb: T => JAXBElement[_]): Conversion[T, Document] = {
    Conversion.build[T, Document] { message =>
      tryEither {
        val document = DocumentBuilderFactory.newInstance().tap(_.setNamespaceAware(true)).newDocumentBuilder().newDocument()
        val soapEnvelope = document.createElementNS(SoapNamespaceUri, "soapenv:Envelope").tap(document.appendChild)
        soapEnvelope.setAttributeNS("http://www.w3.org/2000/xmlns/", "xmlns:head", NsiFrameworkHeaderNamespace)
        soapEnvelope.setAttributeNS("http://www.w3.org/2000/xmlns/", "xmlns:type", NsiConnectionTypesNamespace)
        soapEnvelope.setAttributeNS("http://www.w3.org/2000/xmlns/", "xmlns:saml", "urn:oasis:names:tc:SAML:2.0:assertion")
        soapEnvelope.setAttributeNS("http://www.w3.org/2000/xmlns/", "xmlns:xmlenc", "http://www.w3.org/2001/04/xmlenc#")
        soapEnvelope.setAttributeNS("http://www.w3.org/2000/xmlns/", "xmlns:xmldsig", "http://www.w3.org/2000/09/xmldsig#")
        val soapHeader = document.createElementNS(SoapNamespaceUri, "soapenv:Header").tap(soapEnvelope.appendChild)
        val soapBody = document.createElementNS(SoapNamespaceUri, "soapenv:Body").tap(soapEnvelope.appendChild)

        val marshaller = jaxbContext.createMarshaller()
        val header = Conversion[NsiHeaders, CommonHeaderType].apply(message.headers).right.get
        marshaller.marshal(headersFactory.createNsiHeader(header), soapHeader)
        Option(soapHeader.getFirstChild()).collect { case element: Element => element.removeAttributeNS("http://www.w3.org/2000/xmlns/", "xmlns") }

        marshaller.marshal(messageToJaxb(message), soapBody)
        Option(soapBody.getFirstChild()).collect { case element: Element => element.removeAttributeNS("http://www.w3.org/2000/xmlns/", "xmlns") }

        document
      }
    } { document =>
      val unmarshaller = jaxbContext.createUnmarshaller()
      for {
        soapEnvelope <- Option(document.getDocumentElement).toRight("missing document root").right
        soapHeader <- onlyChildElementWithNamespaceAndLocalName(SoapNamespaceUri, "Header", soapEnvelope).right
        headerNode <- onlyChildElementWithNamespaceAndLocalName(NsiFrameworkHeaderNamespace, "nsiHeader", soapHeader).right
        soapBody <- onlyChildElementWithNamespaceAndLocalName(SoapNamespaceUri, "Body", soapEnvelope).right
        bodyNode <- onlyChildElementWithNamespace(NsiConnectionTypesNamespace, soapBody).right
        commonHeaderType <- tryEither(unmarshaller.unmarshal(headerNode, classOf[CommonHeaderType]).getValue()).right
        headers <- Conversion[NsiHeaders, CommonHeaderType].invert(commonHeaderType).right
        messageFactory <- elementToFactory(bodyNode).right
        body <- tryEither(unmarshaller.unmarshal(bodyNode, messageFactory.klass).getValue()).right
      } yield {
        messageFactory(headers, body)
      }
    }
  }

  private def onlyChildElementWithNamespace(namespaceUri: String, elem: Element): Either[String, Element] = {
    val childNodes = elem.getChildNodes()
    val children = for (i <- 0 until childNodes.getLength) yield childNodes.item(i)
    children.collect {
      case e: Element if e.getNamespaceURI() == namespaceUri => e
    }.toList match {
      case Nil      => Left(s"missing element in '${elem.getLocalName}', expected exactly one")
      case e :: Nil => Right(e)
      case _        => Left(s"multiple elements in '${elem.getLocalName}', expected exactly one")
    }
  }

  private def onlyChildElementWithNamespaceAndLocalName(namespaceUri: String, localName: String, elem: Element): Either[String, Element] = {
    val childNodes = elem.getChildNodes()
    val children = for (i <- 0 until childNodes.getLength) yield childNodes.item(i)
    children.collect {
      case e: Element if e.getNamespaceURI() == namespaceUri && e.getLocalName() == localName => e
    }.toList match {
      case Nil      => Left(s"missing element in '${elem.getLocalName}', expected exactly one")
      case e :: Nil => Right(e)
      case _        => Left(s"multiple elements in '${elem.getLocalName}', expected exactly one")
    }
  }
}
