package helpers

import akka.util.Timeout

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
abstract class Specification
    extends org.specs2.mutable.Specification
    with org.specs2.mutable.Tags
    with org.specs2.execute.PendingUntilFixed
    with org.specs2.time.NoTimeConversions
    with org.specs2.ScalaCheck
    with play.api.test.DefaultAwaitTimeout
    with play.api.test.FutureAwaits
