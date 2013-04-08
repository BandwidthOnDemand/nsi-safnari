package support

import org.specs2.mutable.Specification
import org.specs2.execute.PendingUntilFixed
import org.junit.runner.RunWith
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Await}
import play.api.mvc._
import play.api.http.HeaderNames.CONTENT_TYPE
import play.api.test._
import play.api.libs.iteratee._
import javax.xml.soap._
import java.io.File
import models.NsiRequestMessage

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class ExtraBodyParsersSpec extends Specification with PendingUntilFixed {

  import ExtraBodyParsers._

  "SoapBodyParser" should {

    "give BadRequest when wrong content-type is set" in {
      val request = FakeRequest().withHeaders(CONTENT_TYPE -> "text/html")
      val result = soap.apply(request).run

      Await.result(result, Duration.Inf) match {
        case Left(SimpleResult(h, _)) => h.status must_== 400
        case x => failure(s"unexpected response $x")
      }
    }

    "give BadRequest when the soap message is not valid" in {
      val result = Enumerator("<nonSoap></nonSoap>".getBytes) |>>> soap.apply(FakeSoapRequest())

      Await.result(result, Duration.Inf) match {
        case Left(SimpleResult(h, _)) => h.status must_== 400
        case x => failure(s"unexpected response $x")
      }
    }

    "give EntityToLarge when the input is to large" in {
      val result = Enumerator("<test></test>".getBytes) |>>> soap(10).apply(FakeSoapRequest())

      Await.result(result, Duration.Inf) match {
        case Left(SimpleResult(h, _)) => h.status must_== 413
        case x => failure(s"unexpected response $x")
      }
    }

    "give a SOAP message for a valid request" in {
      val result = Enumerator.fromFile(new File("test/reserve.xml")) |>>> soap.apply(FakeSoapRequest())

      Await.result(result, Duration.Inf) match {
        case Right(message) => true
        case x => failure(s"unexpected response $x")
      }
    }
  }

  "NsiRequestParser" should {

    "give NSI Reserve for a valid reserve request" in {
      val result = Enumerator.fromFile(new File("test/reserve.xml")) |>>> nsiRequestMessage.apply(FakeSoapRequest())

      Await.result(result, Duration.Inf) match {
        case Right(_: NsiRequestMessage.Reserve) => true
        case x => failure(s"unexpected response $x")
      }
    }

    "give NSI Reserve for a request with extra xml" in {
      val result = Enumerator.fromFile(new File("test/reserve_additional_xml.xml")) |>>> nsiRequestMessage.apply(FakeSoapRequest())

      Await.result(result, Duration.Inf) match {
        case Right(_: NsiRequestMessage.Reserve) => true
        case x => failure(s"unexpected response $x")
      }
    }

    "give BadRequest when the nsi headers are missing" in {
      val result = Enumerator.fromFile(new File("test/reserve_without_headers.xml")) |>>> nsiRequestMessage.apply(FakeSoapRequest())

      Await.result(result, Duration.Inf) match {
        case Left(SimpleResult(h, _)) => h.status must_== 400
        case x => failure(s"unexpected response $x")
      }
    }.pendingUntilFixed

  }

  object FakeSoapRequest {
    def apply(): FakeRequest[AnyContentAsEmpty.type] = {
      FakeRequest("POST", "/", FakeHeaders(), AnyContentAsEmpty).withHeaders(CONTENT_TYPE -> "text/xml")
    }
  }
}