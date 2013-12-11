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

  val NsiXmlDocumentConversion = XmlDocumentConversion("wsdl/soap/soap-envelope-1.1.xsd", "wsdl/2.0/ogf_nsi_framework_headers_v2_0.xsd", "wsdl/2.0/ogf_nsi_connection_types_v2_0.xsd")

  implicit val DocumentToString: Conversion[Document, String] = NsiXmlDocumentConversion.andThen(ByteArrayToString)

  implicit def NsiToString[T](implicit conversion: Conversion[T, Document]): Conversion[T, String] = conversion.andThen(NsiXmlDocumentConversion).andThen(ByteArrayToString)

  private val typesFactory = new org.ogf.schemas.nsi._2013._07.connection.types.ObjectFactory()
  private val headersFactory = new org.ogf.schemas.nsi._2013._07.framework.headers.ObjectFactory()
  private val pointToPointServiceFactory = new org.ogf.schemas.nsi._2013._07.services.point2point.ObjectFactory()
  private val SchemaPackages = Seq(typesFactory, headersFactory, pointToPointServiceFactory).map(_.getClass().getPackage().getName())

  def NsiProviderMessageToDocument[T](defaultHeaders: Option[NsiHeaders])(implicit bodyConversion: Conversion[T, Element]): Conversion[NsiProviderMessage[T], Document] = (Conversion.build[NsiProviderMessage[T], (Option[NsiHeaders], T)] {
    message => Right((Some(message.headers), message.body))
  } {
    case (headers, body) =>
      headers.orElse(defaultHeaders).toRight("missing NSI headers").right.map(NsiProviderMessage(_, body))
  }).andThen(NsiHeadersAndBodyToDocument[T](bodyConversion))

  def NsiRequesterMessageToDocument[T](defaultHeaders: Option[NsiHeaders])(implicit bodyConversion: Conversion[T, Element]): Conversion[NsiRequesterMessage[T], Document] = (Conversion.build[NsiRequesterMessage[T], (Option[NsiHeaders], T)] {
    message => Right((Some(message.headers), message.body))
  } {
    case (headers, body) =>
      headers.orElse(defaultHeaders).toRight("missing NSI headers").right.map(NsiRequesterMessage(_, body))
  }).andThen(NsiHeadersAndBodyToDocument[T](bodyConversion))

  private def marshal[T](jaxb: JAXBElement[T]): Either[String, Element] = tryEither {
    val result = new DOMResult()
    jaxbContext.createMarshaller().marshal(jaxb, result)
    result.getNode().asInstanceOf[Document].getDocumentElement()
  }

  implicit val NsiAcknowledgementOperationToElement = Conversion.build[NsiAcknowledgement, Element] { ack =>
    ack match {
      case GenericAck() =>
        marshal(typesFactory.createAcknowledgment(new GenericAcknowledgmentType()))
      case ReserveResponse(connectionId) =>
        marshal(typesFactory.createReserveResponse(new ReserveResponseType().withConnectionId(connectionId)))
      case QuerySummarySyncConfirmed(reservations) =>
        marshal(typesFactory.createQuerySummarySyncConfirmed(new QuerySummaryConfirmedType().withReservation(reservations.asJava)))
      case QueryNotificationSyncConfirmed(notifications) =>
        marshal(typesFactory.createQueryNotificationSyncConfirmed(new QueryNotificationConfirmedType().withErrorEventOrReserveTimeoutOrDataPlaneStateChange(notifications.asJava)))
      case QueryNotificationSyncFailed(fault) =>
        marshal(typesFactory.createQueryNotificationSyncFailed(fault))
      case ServiceException(exception) =>
        // Wrap the service exception in a SOAP Fault element using the Java DOM API.
        marshal(typesFactory.createServiceException(exception)).right.flatMap { detailBody =>
          tryEither {
            val doc = createDocument
            val fault = doc.createElementNS("http://www.w3.org/2003/05/soap-envelope", "S:Fault").tap(doc.appendChild)
            fault.appendChild(doc.createElement("faultcode")).appendChild(doc.createTextNode("S:Server")) // FIXME or S:Client?
            fault.appendChild(doc.createElement("faultstring")).appendChild(doc.createTextNode(exception.getText()))
            fault.appendChild(doc.createElement("detail")).appendChild(doc.adoptNode(detailBody))
            doc.getDocumentElement()
          }
        }
    }
  } {
    messageFactories(Map[String, NsiMessageParser[NsiAcknowledgement]](
      "acknowledgment" -> NsiMessageParser { (body: GenericAcknowledgmentType) =>
        Right(GenericAck()) },
      "reserveResponse" -> NsiMessageParser { (body: ReserveResponseType) =>
        Right(ReserveResponse(body.getConnectionId())) },
      "querySummarySyncConfirmed" -> NsiMessageParser { (body: QuerySummaryConfirmedType) =>
        Right(QuerySummarySyncConfirmed(body.getReservation().asScala.toVector)) },
      "queryNotificationSyncConfirmed" -> NsiMessageParser { (body: QueryNotificationConfirmedType) =>
        Right(QueryNotificationSyncConfirmed(body.getErrorEventOrReserveTimeoutOrDataPlaneStateChange().asScala.toVector)) },
      "queryNotificationSyncFailed" -> NsiMessageParser { (body: org.ogf.schemas.nsi._2013._07.connection.provider.QueryNotificationSyncFailed) =>
        Right(QueryNotificationSyncFailed(body.getFaultInfo())) },
      "serviceException" -> NsiMessageParser { (body: ServiceExceptionType) =>
        Right(ServiceException(body)) }))
  }

  implicit val NsiProviderOperationToElement = Conversion.build[NsiProviderOperation, Element] { operation =>
    marshal(operation match {
      case InitialReserve(body, _, _)                      => typesFactory.createReserve(body)
      case ReserveCommit(connectionId)                     => typesFactory.createReserveCommit(new GenericRequestType().withConnectionId(connectionId))
      case ReserveAbort(connectionId)                      => typesFactory.createReserveAbort(new GenericRequestType().withConnectionId(connectionId))
      case Provision(connectionId)                         => typesFactory.createProvision(new GenericRequestType().withConnectionId(connectionId))
      case Release(connectionId)                           => typesFactory.createRelease(new GenericRequestType().withConnectionId(connectionId))
      case Terminate(connectionId)                         => typesFactory.createTerminate(new GenericRequestType().withConnectionId(connectionId))
      case QuerySummary(ids)                               => typesFactory.createQuerySummary(toQueryType(ids))
      case QuerySummarySync(ids)                           => typesFactory.createQuerySummarySync(toQueryType(ids))
      case QueryRecursive(ids)                             => typesFactory.createQueryRecursive(toQueryType(ids))
      case QueryNotification(connectionId, start, end)     => typesFactory.createQueryNotification(new QueryNotificationType()
                                                                .withConnectionId(connectionId)
                                                                .withStartNotificationId(start.map(x => x: Integer).orNull)
                                                                .withEndNotificationId(end.map(x => x: Integer).orNull))
      case QueryNotificationSync(connectionId, start, end) => typesFactory.createQueryNotificationSync(new QueryNotificationType()
                                                                .withConnectionId(connectionId)
                                                                .withStartNotificationId(start.map(x => x: Integer).orNull)
                                                                .withEndNotificationId(end.map(x => x: Integer).orNull))
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
      "reserveCommit" -> NsiMessageParser { body: GenericRequestType => Right(ReserveCommit(body.getConnectionId())) },
      "reserveAbort" -> NsiMessageParser { body: GenericRequestType => Right(ReserveAbort(body.getConnectionId())) },
      "provision" -> NsiMessageParser { body: GenericRequestType => Right(Provision(body.getConnectionId())) },
      "release" -> NsiMessageParser { body: GenericRequestType => Right(Release(body.getConnectionId())) },
      "terminate" -> NsiMessageParser { body: GenericRequestType => Right(Terminate(body.getConnectionId())) },
      "querySummary" -> NsiMessageParser { body: QueryType => Right(QuerySummary(toIds(body))) },
      "querySummarySync" -> NsiMessageParser { body: QueryType => Right(QuerySummarySync(toIds(body))) },
      "queryRecursive" -> NsiMessageParser { body: QueryType => Right(QueryRecursive(toIds(body))) },
      "queryNotification" -> NsiMessageParser { body: QueryNotificationType =>
        Right(QueryNotification(body.getConnectionId,
          Option(body.getStartNotificationId()).map(_.toInt),
          Option(body.getEndNotificationId()).map(_.toInt))) },
      "queryNotificationSync" -> NsiMessageParser { body: QueryNotificationType =>
        Right(QueryNotificationSync(body.getConnectionId,
          Option(body.getStartNotificationId()).map(_.toInt),
          Option(body.getEndNotificationId()).map(_.toInt))) }))
  }

  private def toIds(query: QueryType): Option[Either[Seq[ConnectionId], Seq[GlobalReservationId]]] =
    if (!query.getConnectionId().isEmpty())
      Some(Left(query.getConnectionId().asScala))
    else if (!query.getGlobalReservationId().isEmpty)
      Some(Right(query.getGlobalReservationId().asScala.map(URI.create)))
    else
      None

  private def toQueryType(ids: Option[Either[Seq[ConnectionId], Seq[GlobalReservationId]]]): QueryType = ids match {
    case Some(Left(connectionIds))         => new QueryType().withConnectionId(connectionIds.asJava)
    case Some(Right(globalReservationIds)) => new QueryType().withGlobalReservationId(globalReservationIds.map(_.toASCIIString()).asJava)
    case None                              => new QueryType()
  }

  implicit val NsiRequesterOperationToElement = Conversion.build[NsiRequesterOperation, Element] { operation =>
    marshal(operation match {
      case ReserveConfirmed(connectionId, criteria)  => typesFactory.createReserveConfirmed(new ReserveConfirmedType().withConnectionId(connectionId).withCriteria(criteria))
      case ReserveFailed(failure)                    => typesFactory.createReserveFailed(failure)
      case ReserveCommitConfirmed(connectionId)      => typesFactory.createReserveCommitConfirmed(new GenericConfirmedType().withConnectionId(connectionId))
      case ReserveCommitFailed(failure)              => typesFactory.createReserveCommitFailed(failure)
      case ReserveAbortConfirmed(connectionId)       => typesFactory.createReserveAbortConfirmed(new GenericConfirmedType().withConnectionId(connectionId))
      case ReserveTimeout(timeout)                   => typesFactory.createReserveTimeout(timeout)
      case ProvisionConfirmed(connectionId)          => typesFactory.createProvisionConfirmed(new GenericConfirmedType().withConnectionId(connectionId))
      case ReleaseConfirmed(connectionId)            => typesFactory.createReleaseConfirmed(new GenericConfirmedType().withConnectionId(connectionId))
      case TerminateConfirmed(connectionId)          => typesFactory.createTerminateConfirmed(new GenericConfirmedType().withConnectionId(connectionId))
      case QuerySummaryConfirmed(reservations)       => typesFactory.createQuerySummaryConfirmed(new QuerySummaryConfirmedType().withReservation(reservations.asJava))
      case QuerySummaryFailed(failed)                => typesFactory.createQuerySummaryFailed(failed)
      case QueryRecursiveConfirmed(reservations)     => typesFactory.createQueryRecursiveConfirmed(new QueryRecursiveConfirmedType().withReservation(reservations.asJava))
      case QueryRecursiveFailed(failed)              => typesFactory.createQueryRecursiveFailed(failed)
      case QueryNotificationConfirmed(notifications) => typesFactory.createQueryNotificationConfirmed(new QueryNotificationConfirmedType().withErrorEventOrReserveTimeoutOrDataPlaneStateChange(notifications.asJava))
      case DataPlaneStateChange(notification)        => typesFactory.createDataPlaneStateChange(notification)
      case ErrorEvent(error)                         => typesFactory.createErrorEvent(error)
      case MessageDeliveryTimeout(timeout)           => typesFactory.createMessageDeliveryTimeout(timeout)
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
      "queryNotificationConfirmed" -> NsiMessageParser { body: QueryNotificationConfirmedType => Right(QueryNotificationConfirmed(body.getErrorEventOrReserveTimeoutOrDataPlaneStateChange().asScala.toVector)) },
      "dataPlaneStateChange" -> NsiMessageParser { (body: DataPlaneStateChangeRequestType) => Right(DataPlaneStateChange(body)) },
      "errorEvent" -> NsiMessageParser { (body: ErrorEventType) => Right(ErrorEvent(body)) },
      "messageDeliveryTimeout" -> NsiMessageParser { (body: MessageDeliveryTimeoutRequestType) => Right(MessageDeliveryTimeout(body)) }))
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

  private implicit val NsiHeadersToElement: Conversion[NsiHeaders, Element] = Conversion.build[NsiHeaders, Element] {
    headers =>
      for {
        commonHeaderType <- Conversion[NsiHeaders, CommonHeaderType].apply(headers).right
        headersElement <- marshal(headersFactory.createNsiHeader(commonHeaderType)).right
      } yield headersElement
  } {
    headersElement =>
      val unmarshaller = jaxbContext.createUnmarshaller()
      for {
        commonHeaderType <- tryEither(unmarshaller.unmarshal(headersElement, classOf[CommonHeaderType]).getValue).right
        headers <- Conversion[NsiHeaders, CommonHeaderType].invert(commonHeaderType).right
      } yield headers
  }

  private def NsiJaxbElementToString[T](jaxb: JAXBElement[T]): Conversion[T, String] = Conversion.build[T, String] { value =>
    tryEither {
      val wrapped = new JAXBElement(jaxb.getName(), jaxb.getDeclaredType(), value)
      val marshaller = jaxbContext.createMarshaller()
      val baos = new ByteArrayOutputStream()
      marshaller.marshal(wrapped, baos)
      baos.toString("UTF-8")
    }
  } { s =>
    tryEither {
      val unmarshaller = jaxbContext.createUnmarshaller()
      val source = new StreamSource(new ByteArrayInputStream(s.getBytes("UTF-8")))
      unmarshaller.unmarshal(source, jaxb.getDeclaredType()).getValue()
    }
  }

  implicit val NsiHeadersToXmlString: Conversion[NsiHeaders, String] = Conversion[NsiHeaders, CommonHeaderType] andThen NsiJaxbElementToString(headersFactory.createNsiHeader(null))
  implicit val ServiceExceptionTypeToXmlString: Conversion[ServiceExceptionType, String] = NsiJaxbElementToString(typesFactory.createServiceException(null))

  private def NsiHeadersAndBodyToDocument[T](implicit bodyConversion: Conversion[T, Element]) = Conversion.build[(Option[NsiHeaders], T), Document] {
    case (headers, body) =>
      for {
        headersElementOption <- headers.map(Conversion[NsiHeaders, Element].apply).sequence.right
        bodyElement <- bodyConversion(body).right
        document <- tryEither {
          val document = createDocument
          val soapEnvelope = document.appendChild(document.createElementNS(SoapNamespaceUri, "soapenv:Envelope"))
          val soapHeader = soapEnvelope.appendChild(document.createElementNS(SoapNamespaceUri, "soapenv:Header"))
          val soapBody = soapEnvelope.appendChild(document.createElementNS(SoapNamespaceUri, "soapenv:Body"))

          headersElementOption.foreach(it => soapHeader.appendChild(document.importNode(it, true)))
          soapBody.appendChild(document.importNode(bodyElement, true))

          document
        }.right
      } yield document
  } { document =>
    for {
      soapEnvelope <- Option(document.getDocumentElement).toRight("missing document root").right
      header <- parseNsiHeaders(soapEnvelope).right
      soapBody <- findSingleChildElement(SoapNamespaceUri, "Body", soapEnvelope).right
      soapFaultNode <- findOptionalChildElement(SoapNamespaceUri, "Fault", soapBody).right
      body <- soapFaultNode.map(n => parseSoapFault(n)(bodyConversion)).getOrElse(parseSoapBody(soapBody)(bodyConversion)).right
    } yield {
      (header, body)
    }
  }

  private def parseNsiHeaders(soapEnvelope: Element): Either[String, Option[NsiHeaders]] = for {
      soapHeader <- findOptionalChildElement(SoapNamespaceUri, "Header", soapEnvelope).right
      headerNode <- soapHeader.map(it => findOptionalChildElement(NsiHeadersQName.getNamespaceURI(), NsiHeadersQName.getLocalPart(), it)).getOrElse(Right(None)).right
      header <- headerNode.map(Conversion[NsiHeaders, Element].invert(_)).sequence.right
  } yield header

  private def parseSoapBody[T](soapBody: Element)(implicit bodyConversion: Conversion[T, Element]): Either[String, T] = for {
      bodyNode <- findSingleChildElement(NsiConnectionTypesNamespace, "*", soapBody).right
      body <- bodyConversion.invert(bodyNode).right
  } yield body

  private val ServiceExceptionTypeName = typesFactory.createServiceException(null).getName()

  private def parseSoapFault[T](soapFault: Element)(implicit bodyConversion: Conversion[T, Element]): Either[String, T] = for {
    faultString <- findSingleChildElement("", "faultstring", soapFault).right
    serviceExceptionNode <- findOptionalChildElement(ServiceExceptionTypeName.getNamespaceURI(), ServiceExceptionTypeName.getLocalPart(), soapFault).right
    serviceException <- serviceExceptionNode.map(bodyConversion.invert(_)).sequence.right
    result <- serviceException.toRight(s"SOAP fault without ${ServiceExceptionTypeName}. Fault string: ${faultString.getTextContent()}").right
  } yield result

  private def createDocument: Document = DocumentBuilderFactory.newInstance().tap(_.setNamespaceAware(true)).newDocumentBuilder().newDocument()

  private def findSingleChildElement(namespaceUri: String, localName: String, parent: Element): Either[String, Element] = {
    findOptionalChildElement(namespaceUri, localName, parent) match {
      case Right(None)    => Left(s"missing element '$namespaceUri:$localName' in '${parent.getLocalName}', expected exactly one")
      case Right(Some(x)) => Right(x)
      case Left(x)        => Left(x)
    }
  }

  private def findOptionalChildElement(namespaceUri: String, localName: String, parent: Element): Either[String, scala.Option[Element]] = {
    val childNodes = parent.getElementsByTagNameNS(namespaceUri, localName)
    val children = Vector.tabulate(childNodes.getLength)(childNodes.item)
    children.collect {
      case e: Element => e
    } match {
      case Vector(e) => Right(Some(e))
      case Vector()  => Right(None)
      case _         => Left(s"multiple elements '$namespaceUri:$localName' in '${parent.getLocalName}', expected exactly one")
    }
  }
}
