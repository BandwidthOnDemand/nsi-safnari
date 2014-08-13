package controllers

import java.util.Locale

import org.joda.time.DateTime
import play.api.http.HeaderNames
import play.api.mvc.Results
import play.api.test.{FakeApplication, FakeRequest, PlaySpecification, WithApplication}

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class DiscoveryServiceSpec extends PlaySpecification with Results {

  def Application = FakeApplication(additionalConfiguration = Map("nsi.actor" -> "dummy", "pce.actor" -> "dummy"))

  "Discovery service" should {

    "serve discovery document with fake reachability" in new WithApplication(Application) {
      val controller = new DiscoveryService(PathComputationEngine.pceRequester)

      val result = controller.index().apply(FakeRequest())

      headers(result) must haveKey(HeaderNames.LAST_MODIFIED)

      val body = contentAsString(result)
      body must contain("""<Topology id="urn:ogf:network:surfnet.nl:1990:nsa:bod-dev" cost="0"/>""")
    }

    "return a not modified if if-modified-since header is in the future" in new WithApplication(Application) {
      val controller = new DiscoveryService(PathComputationEngine.pceRequester)

      val dateInTheFuture = DateTime.now().plusHours(5).toString("EEE, dd MMM yyyy HH:mm:ss 'GMT'", Locale.ENGLISH)

      val result = controller.index().apply(FakeRequest().withHeaders(HeaderNames.IF_MODIFIED_SINCE -> dateInTheFuture))

      status(result) must beEqualTo(NOT_MODIFIED)
    }
  }
}