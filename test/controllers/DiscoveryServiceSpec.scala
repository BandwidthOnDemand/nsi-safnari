package controllers

import play.api.test.WithApplication
import play.api.test.PlaySpecification
import play.api.mvc.Results
import play.api.mvc.Controller
import play.api.test.FakeRequest
import play.api.test.FakeApplication
import play.api.http.HeaderNames
import org.joda.time.DateTime
import java.util.Locale

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class DiscoveryServiceSpec extends PlaySpecification with Results {

  class SubjectController extends Controller with DiscoveryService
  def Application = FakeApplication(additionalConfiguration = Map("nsi.actor" -> "dummy", "pce.actor" -> "dummy"))

  "Discovery service" should {

    "serve discovery document with fake reachability" in new WithApplication(Application) {
      val controller = new SubjectController()

      val result = controller.index().apply(FakeRequest())

      headers(result) must haveKey(HeaderNames.LAST_MODIFIED)

      val body = contentAsString(result)
      body must contain("""<Topology id="urn:ogf:network:surfnet.nl:1990:nsa:bod-dev" cost="0"/>""")
    }

    "return a not modified if if-modified-since header is in the future" in new WithApplication(Application) {
      val controller = new SubjectController()

      val dateInTheFuture = DateTime.now().plusHours(5).toString("EEE, dd MMM yyyy HH:mm:ss 'GMT'", Locale.ENGLISH)

      val result = controller.index().apply(FakeRequest().withHeaders(HeaderNames.IF_MODIFIED_SINCE -> dateInTheFuture))

      status(result) must beEqualTo(NOT_MODIFIED)
    }
  }
}