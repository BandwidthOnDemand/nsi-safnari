package controllers

import java.time.{ZoneId, ZonedDateTime}
import java.time.format.DateTimeFormatter
import java.time.temporal.*
import play.api.http.HeaderNames
import play.api.test.{FakeRequest, PlaySpecification, WithApplication}
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.mvc.*
import play.api

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class DiscoveryServiceSpec extends PlaySpecification with Results:

  def Application: api.Application = new GuiceApplicationBuilder()
    .configure(Map("nsi.actor" -> "dummy", "pce.actor" -> "dummy"))
    .build()

  "Discovery service" should {

    "serve discovery document with fake reachability" in new WithApplication(Application):
      override def running() =
        val controller = app.injector.instanceOf[DiscoveryService]
        controller.setControllerComponents(app.injector.instanceOf[ControllerComponents])

        val result = controller.index.apply(FakeRequest())

        headers(result) must haveKey(HeaderNames.LAST_MODIFIED)

        val body = contentAsString(result)
        body must contain(
          """<Topology id="urn:ogf:network:surfnet.nl:1990:nsa:bod-dev" cost="0"/>"""
        )

    "return a not modified if if-modified-since header is in the future" in new WithApplication(
      Application
    ):
      override def running() =
        val dateTimeFormatter = DateTimeFormatter.RFC_1123_DATE_TIME
          .withLocale(java.util.Locale.ENGLISH)
          .withZone(ZoneId.of("GMT"))
        val controller = app.injector.instanceOf[DiscoveryService]
        controller.setControllerComponents(app.injector.instanceOf[ControllerComponents])

        val dateInTheFuture = dateTimeFormatter.format(ZonedDateTime.now.plus(5, ChronoUnit.HOURS))

        val result = controller.index
          .apply(FakeRequest().withHeaders(HeaderNames.IF_MODIFIED_SINCE -> dateInTheFuture))

        status(result) must beEqualTo(NOT_MODIFIED)
  }
end DiscoveryServiceSpec
