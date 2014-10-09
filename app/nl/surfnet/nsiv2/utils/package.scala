package nl.surfnet.nsiv2

import java.net.URI

import scala.util.{Failure, Success, Try}

package object utils {
  def classpathResourceUri(name: String): URI = {
    val classLoader = Thread.currentThread().getContextClassLoader
    val resource = classLoader.getResource(name)
    if (resource != null) resource.toURI
    else throw new IllegalArgumentException(f"classpath resource '$name' not found")
  }

  implicit class OptionOps[A](value: Option[A]) {
    def toTry(ifNone: => Throwable): Try[A] = value.map(Success(_)).getOrElse(Failure(ifNone))
    def toTry(ifNone: String): Try[A] = value.map(Success(_)).getOrElse(Failure(ErrorMessage(ifNone)))
  }

  implicit class OptionTryOps[A](value: Option[Try[A]]) {
    def sequence: Try[Option[A]] = value match {
      case None    => Success(None)
      case Some(t) => t.map(Some(_))
    }
  }

  implicit class TryOps[A](a: Try[A]) {
    def toEither: Either[Throwable, A] = a match {
      case Failure(t) => Left(t)
      case Success(a) => Right(a)
    }
  }
}
