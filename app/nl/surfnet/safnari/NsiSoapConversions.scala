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
import javax.xml.transform.dom.DOMResult

object NsiSoapConversions {
  implicit val ByteArrayToString = Conversion.build[Array[Byte], String] { bytes =>
    tryEither(new String(bytes, "UTF-8"))
  } { string =>
    tryEither(string.getBytes("UTF-8"))
  }

  implicit def NsiToString[T](implicit conversion: Conversion[T, Document]): Conversion[T, String] = conversion.andThen(NsiXmlDocumentConversion).andThen(ByteArrayToString)

  private val typesFactory = new org.ogf.schemas.nsi._2013._07.connection.types.ObjectFactory()
  private val headersFactory = new org.ogf.schemas.nsi._2013._07.framework.headers.ObjectFactory()
  private val pointToPointServiceFactory = new org.ogf.schemas.nsi._2013._07.services.point2point.ObjectFactory()
  private val SchemaPackages = Seq(typesFactory, headersFactory, pointToPointServiceFactory).map(_.getClass().getPackage().getName())

  implicit def NsiProviderMessageToDocument[T](implicit bodyConversion: Conversion[T, Element]): Conversion[NsiProviderMessage[T], Document] = (Conversion.build[NsiProviderMessage[T], (NsiHeaders, T)] {
    message => Right((message.headers, message.body))
  } {
    case (headers, body) => Right(NsiProviderMessage(headers, body))
  }).andThen(NsiHeadersAndBodyToDocument[T](bodyConversion))

  implicit def NsiRequesterMessageToDocument[T](implicit bodyConversion: Conversion[T, Element]): Conversion[NsiRequesterMessage[T], Document] = (Conversion.build[NsiRequesterMessage[T], (NsiHeaders, T)] {
    message => Right((message.headers, message.body))
  } {
    case (headers, body) => Right(NsiRequesterMessage(headers, body))
  }).andThen(NsiHeadersAndBodyToDocument[T](bodyConversion))

  private def marshal[T](jaxb: JAXBElement[T]): Either[String, Element] = tryEither {
    val result = new DOMResult()
    jaxbContext.createMarshaller().marshal(jaxb, result)
    result.getNode().asInstanceOf[Document].getDocumentElement()
  }

  implicit val NsiAcknowledgementOperationToJaxbElement = Conversion.build[NsiAcknowledgement, Element] { ack =>
    marshal(ack match {
      case GenericAck()                            => typesFactory.createAcknowledgment(new GenericAcknowledgmentType())
      case ReserveResponse(connectionId)           => typesFactory.createReserveResponse(new ReserveResponseType().withConnectionId(connectionId))
      case ServiceException(exception)             => typesFactory.createServiceException(exception)
      case QuerySummarySyncConfirmed(reservations) => typesFactory.createQuerySummarySyncConfirmed(new QuerySummaryConfirmedType().withReservation(reservations.asJava))
    })
  } {
    messageFactories(Map[String, NsiMessageParser[NsiAcknowledgement]](
      "acknowledgment" -> NsiMessageParser { (body: GenericAcknowledgmentType) => Right(GenericAck()) },
      "reserveResponse" -> NsiMessageParser { (body: ReserveResponseType) => Right(ReserveResponse(body.getConnectionId())) },
      "serviceException" -> NsiMessageParser { (body: ServiceExceptionType) => Right(ServiceException(body)) },
      "querySummarySyncConfirmed" -> NsiMessageParser { (body: QuerySummaryConfirmedType) => Right(QuerySummarySyncConfirmed(body.getReservation().asScala.to[Vector])) }))
  }

  implicit val NsiProviderOperationToJaxbElement = Conversion.build[NsiProviderOperation, Element] { operation =>
    marshal(operation match {
      case InitialReserve(body, _, _)      => typesFactory.createReserve(body)
      case ReserveCommit(connectionId)     => typesFactory.createReserveCommit(new GenericRequestType().withConnectionId(connectionId))
      case ReserveAbort(connectionId)      => typesFactory.createReserveAbort(new GenericRequestType().withConnectionId(connectionId))
      case Provision(connectionId)         => typesFactory.createProvision(new GenericRequestType().withConnectionId(connectionId))
      case Release(connectionId)           => typesFactory.createRelease(new GenericRequestType().withConnectionId(connectionId))
      case Terminate(connectionId)         => typesFactory.createTerminate(new GenericRequestType().withConnectionId(connectionId))
      case QuerySummary(connectionIds)     => typesFactory.createQuerySummary(new QueryType().withConnectionId(connectionIds.asJava))
      case QuerySummarySync(connectionIds) => typesFactory.createQuerySummarySync(new QueryType().withConnectionId(connectionIds.asJava))
      case QueryRecursive(connectionIds)   => typesFactory.createQueryRecursive(new QueryType().withConnectionId(connectionIds.asJava))
    })
  } {
    messageFactories(Map[String, NsiMessageParser[NsiProviderOperation]](
      "reserve" -> NsiMessageParser { (body: ReserveType) =>
        if (body.getConnectionId ne null) Left("modify operation is not supported")
        else for {
          criteria <- Conversion.invert(body.getCriteria()).right
          service <- criteria.getPointToPointService().toRight("initial reserve is missing point2point service").right
        } yield {
          InitialReserve(body, criteria, service)
        }
      },
      "reserveCommit" -> NsiMessageParser { (body: GenericRequestType) => Right(ReserveCommit(body.getConnectionId())) },
      "reserveAbort" -> NsiMessageParser { (body: GenericRequestType) => Right(ReserveAbort(body.getConnectionId())) },
      "provision" -> NsiMessageParser { (body: GenericRequestType) => Right(Provision(body.getConnectionId())) },
      "release" -> NsiMessageParser { (body: GenericRequestType) => Right(Release(body.getConnectionId())) },
      "terminate" -> NsiMessageParser { (body: GenericRequestType) => Right(Terminate(body.getConnectionId())) },
      "querySummary" -> NsiMessageParser { (body: QueryType) => Right(QuerySummary(body.getConnectionId().asScala)) },
      "querySummarySync" -> NsiMessageParser { (body: QueryType) => Right(QuerySummarySync(body.getConnectionId().asScala)) }))
  }

  implicit val NsiRequesterOperationToJaxbElement = Conversion.build[NsiRequesterOperation, Element] { operation =>
    marshal(operation match {
      case ReserveConfirmed(connectionId, criteria)         => typesFactory.createReserveConfirmed(new ReserveConfirmedType().withConnectionId(connectionId).withCriteria(criteria))
      case ReserveFailed(failure)                           => typesFactory.createReserveFailed(failure)
      case ReserveCommitConfirmed(connectionId)             => typesFactory.createReserveCommitConfirmed(new GenericConfirmedType().withConnectionId(connectionId))
      case ReserveCommitFailed(failure)                     => typesFactory.createReserveCommitFailed(failure)
      case ReserveAbortConfirmed(connectionId)              => typesFactory.createReserveAbortConfirmed(new GenericConfirmedType().withConnectionId(connectionId))
      case ReserveTimeout(timeout)                          => typesFactory.createReserveTimeout(timeout)
      case ProvisionConfirmed(connectionId)                 => typesFactory.createProvisionConfirmed(new GenericConfirmedType().withConnectionId(connectionId))
      case ReleaseConfirmed(connectionId)                   => typesFactory.createReleaseConfirmed(new GenericConfirmedType().withConnectionId(connectionId))
      case TerminateConfirmed(connectionId)                 => typesFactory.createTerminateConfirmed(new GenericConfirmedType().withConnectionId(connectionId))
      case QuerySummaryConfirmed(reservations)              => typesFactory.createQuerySummaryConfirmed(new QuerySummaryConfirmedType().withReservation(reservations.asJava))
      case QuerySummaryFailed(failed)                       => typesFactory.createQuerySummaryFailed(failed)
      case QueryRecursiveConfirmed(reservations)            => typesFactory.createQueryRecursiveConfirmed(new QueryRecursiveConfirmedType().withReservation(reservations.asJava))
      case QueryRecursiveFailed(failed)                     => typesFactory.createQueryRecursiveFailed(failed)
      case DataPlaneStateChange(notification)               => typesFactory.createDataPlaneStateChange(notification)
      case ErrorEvent(error)                                => typesFactory.createErrorEvent(error)
      case MessageDeliveryTimeout(correlationId, timeStamp) => typesFactory.createMessageDeliveryTimeout(new MessageDeliveryTimeoutRequestType().withCorrelationId(correlationId.toString).withTimeStamp(timeStamp))
    })
  } {
    messageFactories(Map[String, NsiMessageParser[NsiRequesterOperation]](
      "reserveConfirmed" -> NsiMessageParser { (body: ReserveConfirmedType) => Right(ReserveConfirmed(body.getConnectionId(), body.getCriteria())) },
      "reserveFailed" -> NsiMessageParser { (body: GenericFailedType) => Right(ReserveFailed(body)) },
      "reserveCommitConfirmed" -> NsiMessageParser { (body: GenericConfirmedType) => Right(ReserveCommitConfirmed(body.getConnectionId)) },
      "reserveCommitFailed" -> NsiMessageParser { (body: GenericFailedType) => Right(ReserveCommitFailed(body)) },
      "reserveAbortConfirmed" -> NsiMessageParser { (body: GenericConfirmedType) => Right(ReserveAbortConfirmed(body.getConnectionId)) },
      "reserveTimeout" -> NsiMessageParser { (body: ReserveTimeoutRequestType) => Right(ReserveTimeout(body)) },
      "provisionConfirmed" -> NsiMessageParser { (body: GenericConfirmedType) => Right(ProvisionConfirmed(body.getConnectionId)) },
      "releaseConfirmed" -> NsiMessageParser { (body: GenericConfirmedType) => Right(ReleaseConfirmed(body.getConnectionId)) },
      "terminateConfirmed" -> NsiMessageParser { (body: GenericConfirmedType) => Right(TerminateConfirmed(body.getConnectionId)) },
      "querySummaryConfirmed" -> NsiMessageParser { (body: QuerySummaryConfirmedType) => Right(QuerySummaryConfirmed(body.getReservation().asScala.toVector)) },
      "querySummaryFailed" -> NsiMessageParser { (body: QueryFailedType) => Right(QuerySummaryFailed(body)) },
      "queryRecursiveConfirmed" -> NsiMessageParser { (body: QueryRecursiveConfirmedType) => Right(QueryRecursiveConfirmed(body.getReservation().asScala.toVector)) },
      "queryRecursiveFailed" -> NsiMessageParser { (body: QueryFailedType) => Right(QueryRecursiveFailed(body)) },
      "dataPlaneStateChange" -> NsiMessageParser { (body: DataPlaneStateChangeRequestType) => Right(DataPlaneStateChange(body)) },
      "errorEvent" -> NsiMessageParser { (body: ErrorEventType) => Right(ErrorEvent(body)) },
      "messageDeliveryTimeout" -> NsiMessageParser { (body: MessageDeliveryTimeoutRequestType) =>
        val correlationId = CorrelationId.fromString(body.getCorrelationId()).toRight(s"bad correlation id ${body.getCorrelationId()}")
        correlationId.right.map(MessageDeliveryTimeout(_, body.getTimeStamp()))
      }))
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
    def apply(bodyNode: Element): Either[String, T]
  }
  private object NsiMessageParser {
    def apply[M, T](f: M => Either[String, T])(implicit manifest: ClassTag[M]): NsiMessageParser[T] = new NsiMessageParser[T] {
      override def apply(bodyNode: Element) = {
        val unmarshaller = jaxbContext.createUnmarshaller()
        for {
          body <- tryEither(unmarshaller.unmarshal(bodyNode, manifest.runtimeClass.asInstanceOf[Class[M]])).right
          message <- f(body.getValue()).right
        } yield message
      }
    }
  }

  private def messageFactories[T](factories: Map[String, NsiMessageParser[T]])(bodyNode: Element): Either[String, T] =
    for {
      parser <- factories.get(bodyNode.getLocalName()).toRight(s"unknown body element type '${bodyNode.getLocalName()}'").right
      body <- parser(bodyNode).right
    } yield body

  private val SoapNamespaceUri = "http://schemas.xmlsoap.org/soap/envelope/"
  private val NsiHeadersQName = headersFactory.createNsiHeader(null).getName()
  private val NsiConnectionTypesNamespace = typesFactory.createAcknowledgment(null).getName().getNamespaceURI()

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

  private def NsiHeadersAndBodyToDocument[T](implicit bodyConversion: Conversion[T, Element]): Conversion[(NsiHeaders, T), Document] = Conversion.build[(NsiHeaders, T), Document] {
    case (headers, body) =>
      tryEither {
        val document = DocumentBuilderFactory.newInstance().tap(_.setNamespaceAware(true)).newDocumentBuilder().newDocument()
        val soapEnvelope = document.createElementNS(SoapNamespaceUri, "soapenv:Envelope").tap(document.appendChild)
        soapEnvelope.setAttributeNS("http://www.w3.org/2000/xmlns/", "xmlns:head", NsiHeadersQName.getNamespaceURI())
        soapEnvelope.setAttributeNS("http://www.w3.org/2000/xmlns/", "xmlns:type", NsiConnectionTypesNamespace)
        soapEnvelope.setAttributeNS("http://www.w3.org/2000/xmlns/", "xmlns:saml", "urn:oasis:names:tc:SAML:2.0:assertion")
        soapEnvelope.setAttributeNS("http://www.w3.org/2000/xmlns/", "xmlns:xmlenc", "http://www.w3.org/2001/04/xmlenc#")
        soapEnvelope.setAttributeNS("http://www.w3.org/2000/xmlns/", "xmlns:xmldsig", "http://www.w3.org/2000/09/xmldsig#")
        val soapHeader = document.createElementNS(SoapNamespaceUri, "soapenv:Header").tap(soapEnvelope.appendChild)
        val soapBody = document.createElementNS(SoapNamespaceUri, "soapenv:Body").tap(soapEnvelope.appendChild)

        val marshaller = jaxbContext.createMarshaller()
        val headersJaxb = Conversion[NsiHeaders, CommonHeaderType].apply(headers).right.get
        marshaller.marshal(headersFactory.createNsiHeader(headersJaxb), soapHeader)
        Option(soapHeader.getFirstChild()).collect { case element: Element => element.removeAttributeNS("http://www.w3.org/2000/xmlns/", "xmlns") }

        val bodyElement = bodyConversion(body).right.get
        soapBody.appendChild(document.importNode(bodyElement, true))
        Option(soapBody.getFirstChild()).collect { case element: Element => element.removeAttributeNS("http://www.w3.org/2000/xmlns/", "xmlns") }

        document
      }
  } { document =>
    val unmarshaller = jaxbContext.createUnmarshaller()
    for {
      soapEnvelope <- Option(document.getDocumentElement).toRight("missing document root").right
      soapHeader <- findSingleChildElement(SoapNamespaceUri, "Header", soapEnvelope).right
      headerNode <- findSingleChildElement(NsiHeadersQName.getNamespaceURI(), NsiHeadersQName.getLocalPart(), soapHeader).right
      soapBody <- findSingleChildElement(SoapNamespaceUri, "Body", soapEnvelope).right
      bodyNode <- findSingleChildElement(NsiConnectionTypesNamespace, "*", soapBody).right
      commonHeaderType <- tryEither(unmarshaller.unmarshal(headerNode, classOf[CommonHeaderType]).getValue).right
      header <- Conversion[NsiHeaders, CommonHeaderType].invert(commonHeaderType).right
      body <- bodyConversion.invert(bodyNode).right
    } yield {
      (header, body)
    }
  }

  private def findSingleChildElement(namespaceUri: String, localName: String, parent: Element): Either[String, Element] = {
    val childNodes = parent.getElementsByTagNameNS(namespaceUri, localName)
    val children = Vector.tabulate(childNodes.getLength)(childNodes.item)
    children.collect {
      case e: Element => e
    } match {
      case Vector(e) => Right(e)
      case Vector()  => Left(s"missing element '$namespaceUri:$localName' in '${parent.getLocalName}', expected exactly one")
      case _         => Left(s"multiple elements '$namespaceUri:$localName' in '${parent.getLocalName}', expected exactly one")
    }
  }
}
