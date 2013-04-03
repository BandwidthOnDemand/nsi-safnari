import org.specs2.mutable.Specification
import javax.xml.bind.JAXBContext
import org.ogf.schemas.nsi._2013._04.connection.types.ReserveType
import java.io.File
import javax.xml.soap.MessageFactory
import java.io.FileInputStream
import javax.xml.soap.MimeHeaders
import org.w3c.dom._
import scala.collection.JavaConverters._
import org.junit.runner.RunWith

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class UnmarshallSpec extends Specification {

  "Unmarshaller" should {
    "unmarshall reserve request" in {

      val unmarshaller = JAXBContext.newInstance(classOf[ReserveType]).createUnmarshaller()

      val message = MessageFactory.newInstance().createMessage(new MimeHeaders, new FileInputStream(new File("test/reserveRequest.xml")))

      val reserveRequest = unmarshaller.unmarshal(message.getSOAPBody().getChildElements().asScala.collect {
        case e: Element => e
      }.toSeq.head, classOf[ReserveType]).getValue.pp

      reserveRequest.getDescription() must_== "My first reservation"
    }
  }
}