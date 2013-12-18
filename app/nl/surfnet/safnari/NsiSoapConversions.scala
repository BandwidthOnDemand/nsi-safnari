package nl.surfnet.safnari

import java.io.{ ByteArrayInputStream, ByteArrayOutputStream }
import java.net.URI
import javax.xml.XMLConstants
import javax.xml.bind.JAXBContext
import javax.xml.bind.JAXBElement
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.transform.Source
import javax.xml.transform.TransformerFactory
import javax.xml.transform.dom.DOMResult
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
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import javax.xml.transform.OutputKeys
import scala.xml.parsing.NoBindingFactoryAdapter
import javax.xml.transform.sax.SAXResult

object NsiSoapConversions {
  implicit val ByteArrayToString = Conversion.build[Array[Byte], String] { bytes =>
    Try(new String(bytes, "UTF-8"))
  } { string =>
    Try(string.getBytes("UTF-8"))
  }

  val NsiXmlDocumentConversion = XmlDocumentConversion("wsdl/soap/soap-envelope-1.1.xsd", "wsdl/2.0/ogf_nsi_framework_headers_v2_0.xsd", "wsdl/2.0/ogf_nsi_connection_types_v2_0.xsd")

  def documentToScalaXml(document: Document): scala.xml.Node = {
    val source = new DOMSource(document)
    val adapter = new NoBindingFactoryAdapter
    val saxResult = new SAXResult(adapter)
    val transformerFactory = javax.xml.transform.TransformerFactory.newInstance()
    val transformer = transformerFactory.newTransformer()
    transformer.transform(source, saxResult)
    adapter.rootElem
  }

  implicit val DocumentToString: Conversion[Document, String] = NsiXmlDocumentConversion.andThen(ByteArrayToString)

  implicit def NsiToString[T](implicit conversion: Conversion[T, Document]): Conversion[T, String] = conversion.andThen(NsiXmlDocumentConversion).andThen(ByteArrayToString)

  private val typesFactory = new org.ogf.schemas.nsi._2013._07.connection.types.ObjectFactory()
  private val headersFactory = new org.ogf.schemas.nsi._2013._07.framework.headers.ObjectFactory()
  private val pointToPointServiceFactory = new org.ogf.schemas.nsi._2013._07.services.point2point.ObjectFactory()
  private val SchemaPackages = Seq(typesFactory, headersFactory, pointToPointServiceFactory).map(_.getClass().getPackage().getName())

  def NsiProviderMessageToDocument[T](defaultHeaders: Option[NsiHeaders])(implicit bodyConversion: Conversion[T, Element]): Conversion[NsiProviderMessage[T], Document] = (Conversion.build[NsiProviderMessage[T], (Option[NsiHeaders], T)] {
    message => Success((Some(message.headers), message.body))
  } {
    case (headers, body) =>
      headers.orElse(defaultHeaders).map(headers => Success(NsiProviderMessage(headers, body))).getOrElse(Failure(ErrorMessageException("missing NSI headers")))
  }).andThen(NsiHeadersAndBodyToDocument[T](bodyConversion))

  def NsiRequesterMessageToDocument[T](defaultHeaders: Option[NsiHeaders])(implicit bodyConversion: Conversion[T, Element]): Conversion[NsiRequesterMessage[T], Document] = (Conversion.build[NsiRequesterMessage[T], (Option[NsiHeaders], T)] {
    message => Success((Some(message.headers), message.body))
  } {
    case (headers, body) =>
      headers.orElse(defaultHeaders).map(headers => Success(NsiRequesterMessage(headers, body))).getOrElse(Failure(ErrorMessageException("missing NSI headers")))
  }).andThen(NsiHeadersAndBodyToDocument[T](bodyConversion))

  private def marshal[T](jaxb: JAXBElement[T]): Try[Element] = Try {
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
        marshal(typesFactory.createServiceException(exception)).flatMap { detailBody =>
          Try {
            val doc = createDocument
            val fault = doc.createElementNS("http://www.w3.org/2003/05/soap-envelope", "S:Fault").tap(doc.appendChild)
            fault.appendChild(doc.createElement("faultcode")).appendChild(doc.createTextNode("S:Server")) // FIXME or S:Client?
            fault.appendChild(doc.createElement("faultstring")).appendChild(doc.createTextNode(exception.getText()))
            fault.appendChild(doc.createElement("detail")).appendChild(doc.importNode(detailBody, true))
            doc.getDocumentElement()
          }
        }
    }
  } {
    messageFactories(Map[String, NsiMessageParser[NsiAcknowledgement]](
      "acknowledgment" -> NsiMessageParser { (body: GenericAcknowledgmentType) => Success(GenericAck()) },
      "reserveResponse" -> NsiMessageParser { (body: ReserveResponseType) =>
        Success(ReserveResponse(body.getConnectionId()))
      },
      "querySummarySyncConfirmed" -> NsiMessageParser { (body: QuerySummaryConfirmedType) =>
        Success(QuerySummarySyncConfirmed(body.getReservation().asScala.toVector))
      },
      "queryNotificationSyncConfirmed" -> NsiMessageParser { (body: QueryNotificationConfirmedType) =>
        Success(QueryNotificationSyncConfirmed(body.getErrorEventOrReserveTimeoutOrDataPlaneStateChange().asScala.toVector))
      },
      "queryNotificationSyncFailed" -> NsiMessageParser { (body: org.ogf.schemas.nsi._2013._07.connection.provider.QueryNotificationSyncFailed) =>
        Success(QueryNotificationSyncFailed(body.getFaultInfo()))
      },
      "serviceException" -> NsiMessageParser { (body: ServiceExceptionType) =>
        Success(ServiceException(body))
      }))
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
        if (body.getConnectionId ne null) Failure(ErrorMessageException("modify operation is not supported"))
        else for {
          criteria <- Conversion.invert(body.getCriteria())
          service <- criteria.getPointToPointService().toTry(ErrorMessageException("initial reserve is missing point2point service"))
        } yield {
          InitialReserve(body, criteria, service)
        }
      },
      "reserveCommit" -> NsiMessageParser { body: GenericRequestType => Success(ReserveCommit(body.getConnectionId())) },
      "reserveAbort" -> NsiMessageParser { body: GenericRequestType => Success(ReserveAbort(body.getConnectionId())) },
      "provision" -> NsiMessageParser { body: GenericRequestType => Success(Provision(body.getConnectionId())) },
      "release" -> NsiMessageParser { body: GenericRequestType => Success(Release(body.getConnectionId())) },
      "terminate" -> NsiMessageParser { body: GenericRequestType => Success(Terminate(body.getConnectionId())) },
      "querySummary" -> NsiMessageParser { body: QueryType => Success(QuerySummary(toIds(body))) },
      "querySummarySync" -> NsiMessageParser { body: QueryType => Success(QuerySummarySync(toIds(body))) },
      "queryRecursive" -> NsiMessageParser { body: QueryType => Success(QueryRecursive(toIds(body))) },
      "queryNotification" -> NsiMessageParser { body: QueryNotificationType =>
        Success(QueryNotification(body.getConnectionId,
          Option(body.getStartNotificationId()).map(_.toInt),
          Option(body.getEndNotificationId()).map(_.toInt)))
      },
      "queryNotificationSync" -> NsiMessageParser { body: QueryNotificationType =>
        Success(QueryNotificationSync(body.getConnectionId,
          Option(body.getStartNotificationId()).map(_.toInt),
          Option(body.getEndNotificationId()).map(_.toInt)))
      }))
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
      "reserveConfirmed" -> NsiMessageParser { (body: ReserveConfirmedType) => Success(ReserveConfirmed(body.getConnectionId(), body.getCriteria())) },
      "reserveFailed" -> NsiMessageParser { (body: GenericFailedType) => Success(ReserveFailed(body)) },
      "reserveCommitConfirmed" -> NsiMessageParser { (body: GenericConfirmedType) => Success(ReserveCommitConfirmed(body.getConnectionId)) },
      "reserveCommitFailed" -> NsiMessageParser { (body: GenericFailedType) => Success(ReserveCommitFailed(body)) },
      "reserveAbortConfirmed" -> NsiMessageParser { (body: GenericConfirmedType) => Success(ReserveAbortConfirmed(body.getConnectionId)) },
      "reserveTimeout" -> NsiMessageParser { (body: ReserveTimeoutRequestType) => Success(ReserveTimeout(body)) },
      "provisionConfirmed" -> NsiMessageParser { (body: GenericConfirmedType) => Success(ProvisionConfirmed(body.getConnectionId)) },
      "releaseConfirmed" -> NsiMessageParser { (body: GenericConfirmedType) => Success(ReleaseConfirmed(body.getConnectionId)) },
      "terminateConfirmed" -> NsiMessageParser { (body: GenericConfirmedType) => Success(TerminateConfirmed(body.getConnectionId)) },
      "querySummaryConfirmed" -> NsiMessageParser { (body: QuerySummaryConfirmedType) => Success(QuerySummaryConfirmed(body.getReservation().asScala.toVector)) },
      "querySummaryFailed" -> NsiMessageParser { (body: QueryFailedType) => Success(QuerySummaryFailed(body)) },
      "queryRecursiveConfirmed" -> NsiMessageParser { (body: QueryRecursiveConfirmedType) => Success(QueryRecursiveConfirmed(body.getReservation().asScala.toVector)) },
      "queryRecursiveFailed" -> NsiMessageParser { (body: QueryFailedType) => Success(QueryRecursiveFailed(body)) },
      "queryNotificationConfirmed" -> NsiMessageParser { body: QueryNotificationConfirmedType => Success(QueryNotificationConfirmed(body.getErrorEventOrReserveTimeoutOrDataPlaneStateChange().asScala.toVector)) },
      "dataPlaneStateChange" -> NsiMessageParser { (body: DataPlaneStateChangeRequestType) => Success(DataPlaneStateChange(body)) },
      "errorEvent" -> NsiMessageParser { (body: ErrorEventType) => Success(ErrorEvent(body)) },
      "messageDeliveryTimeout" -> NsiMessageParser { (body: MessageDeliveryTimeoutRequestType) => Success(MessageDeliveryTimeout(body)) }))
  }

  private implicit val NsiHeadersToCommonHeaderType = Conversion.build[NsiHeaders, CommonHeaderType] { headers =>
    Try(new CommonHeaderType()
      .withCorrelationId(headers.correlationId.toString)
      .withReplyTo(headers.replyTo.map(_.toASCIIString()).orNull)
      .withProtocolVersion(headers.protocolVersion.toASCIIString())
      .withProviderNSA(headers.providerNSA)
      .withRequesterNSA(headers.requesterNSA))
  } { header =>
    for {
      correlationId <- CorrelationId.fromString(header.getCorrelationId()).toTry(ErrorMessageException(s"invalid correlation id ${header.getCorrelationId()}"))
      replyTo <- Try(Option(header.getReplyTo()).map(URI.create))
      protocolVersion <- Try(URI.create(header.getProtocolVersion()))
    } yield {
      NsiHeaders(correlationId, header.getRequesterNSA(), header.getProviderNSA(), replyTo, protocolVersion)
    }
  }

  private trait NsiMessageParser[T] {
    def apply(bodyNode: Element): Try[T]
  }
  private object NsiMessageParser {
    def apply[M, T](f: M => Try[T])(implicit manifest: ClassTag[M]): NsiMessageParser[T] = new NsiMessageParser[T] {
      override def apply(bodyNode: Element) = {
        val unmarshaller = jaxbContext.createUnmarshaller()
        for {
          body <- Try(unmarshaller.unmarshal(bodyNode, manifest.runtimeClass.asInstanceOf[Class[M]]))
          message <- f(body.getValue())
        } yield message
      }
    }
  }

  private def messageFactories[T](factories: Map[String, NsiMessageParser[T]])(bodyNode: Element): Try[T] =
    for {
      parser <- factories.get(bodyNode.getLocalName()).toTry(ErrorMessageException(s"unknown body element type '${bodyNode.getLocalName()}'"))
      body <- parser(bodyNode)
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
      Try {
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
      Try {
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
        commonHeaderType <- Conversion[NsiHeaders, CommonHeaderType].apply(headers)
        headersElement <- marshal(headersFactory.createNsiHeader(commonHeaderType))
      } yield headersElement
  } {
    headersElement =>
      val unmarshaller = jaxbContext.createUnmarshaller()
      for {
        commonHeaderType <- Try(unmarshaller.unmarshal(headersElement, classOf[CommonHeaderType]).getValue)
        headers <- Conversion[NsiHeaders, CommonHeaderType].invert(commonHeaderType)
      } yield headers
  }

  private def NsiJaxbElementToString[T](jaxb: JAXBElement[T]): Conversion[T, String] = Conversion.build[T, String] { value =>
    Try {
      val wrapped = new JAXBElement(jaxb.getName(), jaxb.getDeclaredType(), value)
      val marshaller = jaxbContext.createMarshaller()
      val baos = new ByteArrayOutputStream()
      marshaller.marshal(wrapped, baos)
      baos.toString("UTF-8")
    }
  } { s =>
    Try {
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
        headersElementOption <- headers.map(Conversion[NsiHeaders, Element].apply).sequence
        bodyElement <- bodyConversion(body)
        document <- Try {
          val document = createDocument
          val soapEnvelope = document.appendChild(document.createElementNS(SoapNamespaceUri, "soapenv:Envelope"))
          val soapHeader = soapEnvelope.appendChild(document.createElementNS(SoapNamespaceUri, "soapenv:Header"))
          val soapBody = soapEnvelope.appendChild(document.createElementNS(SoapNamespaceUri, "soapenv:Body"))

          headersElementOption.foreach(it => soapHeader.appendChild(document.importNode(it, true)))
          soapBody.appendChild(document.importNode(bodyElement, true))

          document
        }
      } yield document
  } { document =>
    for {
      soapEnvelope <- Option(document.getDocumentElement).toTry(ErrorMessageException("missing document root"))
      header <- parseNsiHeaders(soapEnvelope)
      soapBody <- findSingleChildElement(SoapNamespaceUri, "Body", soapEnvelope)
      soapFaultNode <- findOptionalChildElement(SoapNamespaceUri, "Fault", soapBody)
      body <- soapFaultNode.map(n => parseSoapFault(n)(bodyConversion)).getOrElse(parseSoapBody(soapBody)(bodyConversion))
    } yield {
      (header, body)
    }
  }

  private def parseNsiHeaders(soapEnvelope: Element): Try[Option[NsiHeaders]] = for {
    soapHeader <- findOptionalChildElement(SoapNamespaceUri, "Header", soapEnvelope)
    headerNode <- soapHeader.map(it => findOptionalChildElement(NsiHeadersQName.getNamespaceURI(), NsiHeadersQName.getLocalPart(), it)).getOrElse(Success(None))
    header <- headerNode.map(Conversion[NsiHeaders, Element].invert(_)).sequence
  } yield header

  private def parseSoapBody[T](soapBody: Element)(implicit bodyConversion: Conversion[T, Element]): Try[T] = for {
    bodyNode <- findSingleChildElement(NsiConnectionTypesNamespace, "*", soapBody)
    body <- bodyConversion.invert(bodyNode)
  } yield body

  private val ServiceExceptionTypeName = typesFactory.createServiceException(null).getName()

  private def parseSoapFault[T](soapFault: Element)(implicit bodyConversion: Conversion[T, Element]): Try[T] = for {
    faultString <- findSingleChildElement("", "faultstring", soapFault)
    serviceExceptionNode <- findOptionalChildElement(ServiceExceptionTypeName.getNamespaceURI(), ServiceExceptionTypeName.getLocalPart(), soapFault)
    serviceException <- serviceExceptionNode.map(bodyConversion.invert(_)).sequence
    result <- serviceException.toTry(ErrorMessageException(s"SOAP fault without ${ServiceExceptionTypeName}. Fault string: ${faultString.getTextContent()}"))
  } yield result

  private def createDocument: Document = DocumentBuilderFactory.newInstance().tap(_.setNamespaceAware(true)).newDocumentBuilder().newDocument()

  private def findSingleChildElement(namespaceUri: String, localName: String, parent: Element): Try[Element] = {
    findOptionalChildElement(namespaceUri, localName, parent) match {
      case Success(None)    => Failure(ErrorMessageException(s"missing element '$namespaceUri:$localName' in '${parent.getLocalName}', expected exactly one"))
      case Success(Some(x)) => Success(x)
      case Failure(x)       => Failure(x)
    }
  }

  private def findOptionalChildElement(namespaceUri: String, localName: String, parent: Element): Try[Option[Element]] = {
    val childNodes = parent.getElementsByTagNameNS(namespaceUri, localName)
    val children = Vector.tabulate(childNodes.getLength)(childNodes.item)
    children.collect {
      case e: Element => e
    } match {
      case Vector(e) => Success(Some(e))
      case Vector()  => Success(None)
      case _         => Failure(ErrorMessageException(s"multiple elements '$namespaceUri:$localName' in '${parent.getLocalName}', expected exactly one"))
    }
  }
}
