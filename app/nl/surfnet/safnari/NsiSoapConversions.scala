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
    messageFactories(Map[String, NsiMessageParser[NsiAcknowledgement]](
      "acknowledgment" -> NsiMessageParser { (headers, body: GenericAcknowledgmentType) => Right(GenericAck(headers)) },
      "reserveResponse" -> NsiMessageParser { (headers, body: ReserveResponseType) => Right(ReserveResponse(headers, body.getConnectionId())) },
      "serviceException" -> NsiMessageParser { (headers, body: ServiceExceptionType) => Right(ServiceException(headers, body)) },
      "querySummarySyncConfirmed" -> NsiMessageParser { (headers, body: QuerySummaryConfirmedType) => Right(QuerySummarySyncConfirmed(headers, body.getReservation().asScala.to[Vector])) }))
  } {
    case GenericAck(_)                              => typesFactory.createAcknowledgment(new GenericAcknowledgmentType())
    case ReserveResponse(_, connectionId)           => typesFactory.createReserveResponse(new ReserveResponseType().withConnectionId(connectionId))
    case ServiceException(_, exception)             => typesFactory.createServiceException(exception)
    case QuerySummarySyncConfirmed(_, reservations) => typesFactory.createQuerySummarySyncConfirmed(new QuerySummaryConfirmedType().withReservation(reservations.asJava))
  }

  implicit val NsiProviderOperationToDocument: Conversion[NsiProviderOperation, Document] = NsiMessageToDocumentConversion {
    messageFactories(Map[String, NsiMessageParser[NsiProviderOperation]](
      "reserve" -> NsiMessageParser { (headers, body: ReserveType) =>
        if (body.getConnectionId ne null) Left("modify operation is not supported")
        else for {
          criteria <- Conversion.invert(body.getCriteria()).right
          service <- criteria.getPointToPointService().toRight("initial reserve is missing point2point service").right
        } yield {
          InitialReserve(headers, body, criteria, service)
        }
      },
      "reserveCommit" -> NsiMessageParser { (headers, body: GenericRequestType) => Right(ReserveCommit(headers, body.getConnectionId())) },
      "reserveAbort" -> NsiMessageParser { (headers, body: GenericRequestType) => Right(ReserveAbort(headers, body.getConnectionId())) },
      "provision" -> NsiMessageParser { (headers, body: GenericRequestType) => Right(Provision(headers, body.getConnectionId())) },
      "release" -> NsiMessageParser { (headers, body: GenericRequestType) => Right(Release(headers, body.getConnectionId())) },
      "terminate" -> NsiMessageParser { (headers, body: GenericRequestType) => Right(Terminate(headers, body.getConnectionId())) },
      "querySummary" -> NsiMessageParser { (headers, body: QueryType) => Right(QuerySummary(headers, body.getConnectionId().asScala)) },
      "querySummarySync" -> NsiMessageParser { (headers, body: QueryType) => Right(QuerySummarySync(headers, body.getConnectionId().asScala)) }))
  } {
    case InitialReserve(_, body, _, _)      => typesFactory.createReserve(body)
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
    messageFactories(Map[String, NsiMessageParser[NsiRequesterOperation]](
      // FIXME this list seems to be incomplete?
      "reserveConfirmed" -> NsiMessageParser { (headers, body: ReserveConfirmedType) => Right(ReserveConfirmed(headers, body.getConnectionId(), body.getCriteria())) },
      "reserveCommitConfirmed" -> NsiMessageParser[GenericConfirmedType, NsiRequesterOperation]((headers, body) => Right(ReserveCommitConfirmed(headers, body.getConnectionId))),
      "reserveTimeout" -> NsiMessageParser[ReserveTimeoutRequestType, NsiRequesterOperation]((headers, body) => Right(ReserveTimeout(headers, body))),
      "provisionConfirmed" -> NsiMessageParser[GenericConfirmedType, NsiRequesterOperation]((headers, body) => Right(ProvisionConfirmed(headers, body.getConnectionId))),
      "releaseConfirmed" -> NsiMessageParser[GenericConfirmedType, NsiRequesterOperation]((headers, body) => Right(ReleaseConfirmed(headers, body.getConnectionId))),
      "terminateConfirmed" -> NsiMessageParser[GenericConfirmedType, NsiRequesterOperation]((headers, body) => Right(TerminateConfirmed(headers, body.getConnectionId))),
      "dataPlaneStateChange" -> NsiMessageParser[DataPlaneStateChangeRequestType, NsiRequesterOperation]((headers, body) => Right(DataPlaneStateChange(headers, body.getConnectionId(), body.getDataPlaneStatus(), body.getTimeStamp())))))
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

  private trait NsiMessageParser[T] {
    def apply(headerNode: Element, bodyNode: Element): Either[String, T]
  }
  private object NsiMessageParser {
    def apply[M, T](f: (NsiHeaders, M) => Either[String, T])(implicit manifest: ClassTag[M]) = new NsiMessageParser[T] {
      override def apply(headerNode: Element, bodyNode: Element) = {
        val unmarshaller = jaxbContext.createUnmarshaller()
        for {
          commonHeaderType <- tryEither(unmarshaller.unmarshal(headerNode, classOf[CommonHeaderType]).getValue).right
          header <- Conversion[NsiHeaders, CommonHeaderType].invert(commonHeaderType).right
          body <- tryEither(unmarshaller.unmarshal(bodyNode, manifest.runtimeClass.asInstanceOf[Class[M]]).getValue).right
          message <- f(header, body).right
        } yield message
      }
    }
  }

  private def messageFactories[T <: NsiMessage](factories: Map[String, NsiMessageParser[T]])(bodyNode: Element): Either[String, NsiMessageParser[T]] =
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

  private def NsiMessageToDocumentConversion[T <: NsiMessage](elementToFactory: Element => Either[String, NsiMessageParser[T]])(messageToJaxb: T => JAXBElement[_]): Conversion[T, Document] = {
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
      for {
        soapEnvelope <- Option(document.getDocumentElement).toRight("missing document root").right
        soapHeader <- onlyChildElementWithNamespaceAndLocalName(SoapNamespaceUri, "Header", soapEnvelope).right
        headerNode <- onlyChildElementWithNamespaceAndLocalName(NsiFrameworkHeaderNamespace, "nsiHeader", soapHeader).right
        soapBody <- onlyChildElementWithNamespaceAndLocalName(SoapNamespaceUri, "Body", soapEnvelope).right
        bodyNode <- onlyChildElementWithNamespace(NsiConnectionTypesNamespace, soapBody).right
        messageFactory <- elementToFactory(bodyNode).right
        message <- messageFactory.apply(headerNode, bodyNode).right
      } yield {
        message
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
