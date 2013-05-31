package nl.surfnet.safnari

/**
 * Defines an invertable conversion that can potentially fail.
 */
trait Conversion[A, B] { outer =>
  def apply(a: A): Either[String, B]
  def invert: Conversion[B, A]

  def andThen[C](that: Conversion[B, C]) = new Conversion[A, C] { inner =>
    override def apply(a: A): Either[String, C] = outer.apply(a).right.flatMap(that.apply)
    override val invert: Conversion[C, A] = new Conversion[C, A] {
      override def apply(c: C): Either[String, A] = that.invert.apply(c).right.flatMap(outer.invert.apply)
      override val invert = inner
    }
  }
}
object Conversion {
  def apply[A, B](implicit conversion: Conversion[A, B]) = conversion
  def build[A, B](to: A => Either[String, B])(from: B => Either[String, A]): Conversion[A, B] = new Conversion[A, B] { outer =>
    override def apply(a: A) = to(a)
    override val invert = new Conversion[B, A] {
      override def apply(b: B) = from(b)
      override val invert = outer
    }
  }

  def convert[A, B](a: A)(implicit conversion: Conversion[A, B]) = conversion(a)
  def invert[A, B](b: B)(implicit conversion: Conversion[A, B]) = conversion.invert(b)
}
