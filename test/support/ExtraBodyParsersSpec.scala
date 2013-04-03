package support

import org.specs2.mutable.Specification
import org.specs2.execute.PendingUntilFixed
import org.junit.runner.RunWith
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Await}
import play.api.mvc._
import play.api.http.HeaderNames.CONTENT_TYPE
import play.api.test.FakeRequest
import play.api.libs.iteratee._
import javax.xml.soap._
import java.io.ByteArrayInputStream
import java.io.FileInputStream
import java.io.File

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class ExtraBodyParsersSpec extends Specification with PendingUntilFixed {

  import ExtraBodyParsers._

  "SoapBodyParser" should {

    "throw exception when creating message" in {
      val message = MessageFactory.newInstance().createMessage(new MimeHeaders, new ByteArrayInputStream("<text></text>".getBytes("UTF-8")))
      message.getSOAPBody() must throwA[SOAPException]
    }

    "give BadRequest when wrong content-type is set" in {
      val request = FakeRequest().withHeaders(CONTENT_TYPE -> "text/html")
      val result = soap.apply(request).run

      Await.result(result, Duration.Inf) match {
        case Left(SimpleResult(h, _)) => h.status must_== 400
        case x => failure(s"unexpected response $x")
      }
    }

    "give BadRequest when the soap message is not valid" in {
      val request = FakeRequest().withHeaders(CONTENT_TYPE -> "text/xml")
      val result = Enumerator("<test></test>".getBytes) |>>> soap.apply(request)

      Await.result(result, Duration.Inf) match {
        case Right(message) =>
          println(message.getSOAPBody())
          true
        case _ => false
      }
    }.pendingUntilFixed
  }

  "give EntityToLarge when the input is to large" in { }
}