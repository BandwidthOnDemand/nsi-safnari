package helpers

import nl.surfnet.nsiv2.messages.CorrelationId
import nl.surfnet.safnari.Uuid

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
abstract class Specification
    extends org.specs2.mutable.Specification
    with org.specs2.matcher.XmlMatchers
    with org.specs2.execute.PendingUntilFixed
    with org.specs2.ScalaCheck
    with play.api.http.HeaderNames
    with play.api.http.Status
    with play.api.test.DefaultAwaitTimeout
    with play.api.test.FutureAwaits
    with play.api.test.ResultExtractors {
  def newCorrelationId = Specification.newCorrelationId

  lazy val ServerPort = sys.props.get("testserver.port").map(_.toInt).getOrElse(19000 + util.Random.nextInt(10000))
}

object Specification {
  private val UuidGenerator = Uuid.randomUuidGenerator

  def newCorrelationId: CorrelationId = CorrelationId.fromUuid(UuidGenerator())
}
