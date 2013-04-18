package nl.surfnet

import java.net.URI
import scala.util.Try
import scala.util.Failure
import scala.util.Success

package object nsi {
  type Message = Any
  type ConnectionId = String
  type CorrelationId = java.util.UUID

  private val UuidGenerator = Uuid.randomUuidGenerator

  def newConnectionId: ConnectionId = UuidGenerator().toString
  def newCorrelationId: CorrelationId = UuidGenerator()

  implicit class AnyOps[A](a: A) {
    def tap(f: A => Unit): A = { f(a); a }
    def pp: A = { Console.err.println(a); a }
    def pp(prefix: String): A = { Console.err.println(s"$prefix: $a"); a }
  }

  def classpathResourceUri(name: String): URI = {
    val classLoader = Thread.currentThread().getContextClassLoader()
    val resource = classLoader.getResource(name)
    if (resource != null) resource.toURI()
    else throw new IllegalArgumentException(f"classpath resource '$name' not found")
  }

  implicit class TryOps[A](a: Try[A]) {
    def toEither: Either[Throwable, A] = a match {
      case Failure(t) => Left(t)
      case Success(a) => Right(a)
    }
  }
}
