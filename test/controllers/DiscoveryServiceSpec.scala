package controllers

import akka.testkit.TestActorRef
import controllers.PathComputationEngine.DummyPceRequesterActor
import java.time.{ ZoneId, ZonedDateTime }
import java.time.format.DateTimeFormatter
import java.time.temporal._
import play.api.http.HeaderNames
import play.api.mvc.Results
import play.api.test.{FakeApplication, FakeRequest, PlaySpecification, WithApplication}
import play.libs.Akka

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class DiscoveryServiceSpec extends PlaySpecification with Results {

  def Application = FakeApplication(additionalConfiguration = Map("nsi.actor" -> "dummy", "pce.actor" -> "dummy"))

  "Discovery service" should {

    "serve discovery document with fake reachability" in new WithApplication(Application) {
      val configuration = app.injector.instanceOf[Configuration]
      implicit val actorSystem = Akka.system
      val pceRequester = TestActorRef[DummyPceRequesterActor]
      val controller = new DiscoveryService(pceRequester, configuration)

      val result = controller.index().apply(FakeRequest())

      headers(result) must haveKey(HeaderNames.LAST_MODIFIED)

      val body = contentAsString(result)
      body must contain("""<Topology id="urn:ogf:network:surfnet.nl:1990:nsa:bod-dev" cost="0"/>""")
    }

    "return a not modified if if-modified-since header is in the future" in new WithApplication(Application) {
      val configuration = app.injector.instanceOf[Configuration]
      implicit val actorSystem = Akka.system
      val pceRequester = TestActorRef[DummyPceRequesterActor]
      val controller = new DiscoveryService(pceRequester, configuration)
      val dateTimeFormatter = DateTimeFormatter.RFC_1123_DATE_TIME.withLocale(java.util.Locale.ENGLISH).withZone(ZoneId.of("GMT"))

      val dateInTheFuture = dateTimeFormatter.format(ZonedDateTime.now.plus(5, ChronoUnit.HOURS))

      val result = controller.index().apply(FakeRequest().withHeaders(HeaderNames.IF_MODIFIED_SINCE -> dateInTheFuture))

      status(result) must beEqualTo(NOT_MODIFIED)
    }
  }
}
