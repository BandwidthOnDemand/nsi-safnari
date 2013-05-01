package support

import org.specs2.mutable.Specification
import org.specs2.execute.PendingUntilFixed
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ Future, Await }
import play.api.mvc._
import play.api.http.HeaderNames.CONTENT_TYPE
import play.api.test._
import play.api.test.Helpers._
import play.api.libs.iteratee._
import javax.xml.soap._
import java.io.File
import nl.surfnet.safnari.NsiProviderOperation
import java.util.concurrent.TimeUnit
import org.specs2.time.NoTimeConversions
import nl.surfnet.safnari.NsiEnvelope
import nl.surfnet.safnari.NsiRequesterOperation
import nl.surfnet.safnari.Reserve
import nl.surfnet.safnari.ReserveConfirmed

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class ExtraBodyParsersSpec extends Specification with PendingUntilFixed with NoTimeConversions {

  import ExtraBodyParsers._

  "SoapBodyParser" should {

    "give BadRequest when wrong content-type is set" in {
      val request = FakeRequest().withHeaders(CONTENT_TYPE -> "text/html")
      val result = await(soap.apply(request).run)

      result must beLeft.like { case result => status(result) must beEqualTo(400) }
    }

    "give BadRequest when the soap message is not valid" in {
      val result = await(Enumerator("<nonSoap></nonSoap>".getBytes) |>>> soap.apply(FakeSoapRequest()))

      result must beLeft.like { case result => status(result) must beEqualTo(400) }
    }

    "give EntityToLarge when the input is to large" in {
      val result = await(Enumerator("<test></test>".getBytes) |>>> soap(10).apply(FakeSoapRequest()))

      result must beLeft.like { case result => status(result) must beEqualTo(413) }
    }

    "give a SOAP message for a valid request" in {
      val result = await(Enumerator.fromFile(new File("test/reserve.xml")) |>>> soap.apply(FakeSoapRequest()))

      result must beRight
    }
  }

  "NsiProviderParser" should {

    "give NSI Reserve for a valid reserve request" in {
      val result = await(Enumerator.fromFile(new File("test/reserve.xml")) |>>> nsiProviderOperation.apply(FakeSoapRequest()))

      result must beRight.like { case NsiEnvelope(_, _: Reserve) => ok }
    }

    "give Badrequest when NSI Reserve contains extra xml" in {
      val result = await(Enumerator.fromFile(new File("test/reserve_additional_xml.xml")) |>>> nsiProviderOperation.apply(FakeSoapRequest()))

      result must beLeft.like {
        case result =>
          status(result) must beEqualTo(400)
          contentAsString(result) must contain("Invalid content was found starting with element 'test'")
      }
    }

    "give BadRequest when the NSI headers are missing" in {
      val result = await(Enumerator.fromFile(new File("test/reserve_without_headers.xml")) |>>> nsiProviderOperation.apply(FakeSoapRequest()))

      result must beLeft.like {
        case result =>
          status(result) must beEqualTo(400)
          contentAsString(result) must beEqualTo("missing NSI element in 'Header', expected one")
      }
    }

    "give BadRequest when there are multiple NSI headers" in {
      val result = await(Enumerator.fromFile(new File("test/reserve_with_duplicate_headers.xml")) |>>> nsiProviderOperation.apply(FakeSoapRequest()))

      result must beLeft.like {
        case result =>
          status(result) must beEqualTo(400)
          contentAsString(result) must beEqualTo("multiple NSI elements in 'Header', expected one")
      }
    }

  }

  "NsiRequesterParser" should {

    "give NSI Reserve Commit for a valid reserve confirmed request" in {
      val result = await(Enumerator.fromFile(new File("test/reserveconfirmed.xml")) |>>> nsiRequesterOperation.apply(FakeSoapRequest()))

      result must beRight.like { case NsiEnvelope(_, _: ReserveConfirmed) => ok }
    }
  }

  object FakeSoapRequest {
    def apply(): FakeRequest[AnyContentAsEmpty.type] = {
      FakeRequest("POST", "/", FakeHeaders(), AnyContentAsEmpty).withHeaders(CONTENT_TYPE -> "text/xml")
    }
  }
}