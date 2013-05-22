package nl.surfnet.safnari

import play.api.libs.json.Format
import org.joda.time.DateTimeUtils
import anorm._
import play.api.db.DB
import play.api.Play.current
import play.api.http.Writeable
import java.nio.charset.Charset
import scala.io.Codec
import com.twitter.bijection.Injection
import org.joda.time.Instant
import javax.xml.soap._
import java.io.ByteArrayOutputStream
import play.api.libs.json.Json

case class StoredMessage(correlationId: CorrelationId, protocol: String, content: String, createdAt: Instant = new Instant())

object StoredMessage {
  def nsiMessageToSoapMessage[T: ToXmlDocument](message: NsiEnvelope[T]): SOAPMessage = {
    val soap = MessageFactory.newInstance().createMessage()

    val header = SOAPFactory.newInstance().createElement(ToXmlDocument[NsiHeaders].asDocument(message.headers).getDocumentElement())

    soap.getSOAPBody().addDocument(ToXmlDocument[T].asDocument(message.body))
    soap.getSOAPHeader().addChildElement(header)
    soap.saveChanges()

    soap
  }

  val NsiMessageToStoredMessage = Injection.build[NsiEnvelope[NsiMessage], StoredMessage] { message =>
    val soap = message match {
      case NsiEnvelope(headers, body: NsiAcknowledgement)    => nsiMessageToSoapMessage(NsiEnvelope(headers, body))
      case NsiEnvelope(headers, body: NsiProviderOperation)  => nsiMessageToSoapMessage(NsiEnvelope(headers, body))
      case NsiEnvelope(headers, body: NsiRequesterOperation) => nsiMessageToSoapMessage(NsiEnvelope(headers, body))
    }
    val content = new ByteArrayOutputStream().tap(soap.writeTo).toString("UTF-8")
    StoredMessage(message.headers.correlationId, "NSIv2", content)
  } { _ =>
    ???
  }

  val PceMessageToStoredMessage = Injection.build[PceMessage, StoredMessage] { message =>
    val content = message match {
      case request: PathComputationRequest => Json.stringify(Json.toJson(request))
      case response: PceResponse           => Json.stringify(Json.toJson(response))
    }
    StoredMessage(message.correlationId, "PCEv1", content)
  } { _ =>
    ???
  }

  implicit val NsiOrPceMessage = Injection.build[Either[NsiEnvelope[NsiMessage], PceMessage], StoredMessage] {
    case Left(nsi)  => NsiMessageToStoredMessage(nsi)
    case Right(pce) => PceMessageToStoredMessage(pce)
  } { stored =>
    stored.protocol match {
      case "NSIv2" => NsiMessageToStoredMessage.invert(stored).map(Left(_))
      case "PCEv1" => PceMessageToStoredMessage.invert(stored).map(Right(_))
    }
  }
}

class MessageStore[T]()(implicit writer: Injection[T, StoredMessage]) {
  def store(aggregatedConnectionId: ConnectionId, message: T) = DB.withTransaction { implicit connection =>
    val stored = writer(message)
    SQL("""
        INSERT INTO messages (aggregated_connection_id, correlation_id, protocol, content, created_at)
             VALUES ({aggregated_connection_id}, {correlation_id}, {protocol}, {content}, {created_at})
        """).on(
      'aggregated_connection_id -> aggregatedConnectionId,
      'correlation_id -> stored.correlationId.value,
      'protocol -> stored.protocol,
      'content -> stored.content,
      'created_at -> new java.sql.Timestamp(stored.createdAt.getMillis())).executeInsert()
  }
}
