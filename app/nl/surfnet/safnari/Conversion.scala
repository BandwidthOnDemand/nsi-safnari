package nl.surfnet.safnari

import scala.util.Try

/**
 * Defines an invertable conversion that can potentially fail.
 */
trait Conversion[A, B] { outer =>
  def apply(a: A): Try[B]
  def invert: Conversion[B, A]

  def andThen[C](that: Conversion[B, C]) = new Conversion[A, C] { inner =>
    override def apply(a: A): Try[C] = outer.apply(a).flatMap(that.apply)
    override val invert: Conversion[C, A] = new Conversion[C, A] {
      override def apply(c: C): Try[A] = that.invert.apply(c).flatMap(outer.invert.apply)
      override val invert = inner
    }
  }
}
object Conversion {
  def apply[A, B](implicit conversion: Conversion[A, B]) = conversion
  def build[A, B](to: A => Try[B])(from: B => Try[A]): Conversion[A, B] = new Conversion[A, B] { outer =>
    override def apply(a: A) = Try(to(a)).flatten
    override val invert = new Conversion[B, A] {
      override def apply(b: B) = Try(from(b)).flatten
      override val invert = outer
    }
  }

  def convert[A, B](a: A)(implicit conversion: Conversion[A, B]) = conversion(a)
  def invert[A, B](b: B)(implicit conversion: Conversion[A, B]) = conversion.invert(b)
}
