package helpers

import akka.util.Timeout

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
abstract class Specification
    extends org.specs2.mutable.Specification
    with org.specs2.execute.PendingUntilFixed
    with org.specs2.time.NoTimeConversions
    with org.specs2.ScalaCheck {

  implicit val timeout = Timeout(2000)

  def testConfiguration = Map("db.default.url" -> "jdbc:h2:mem:test")
}
