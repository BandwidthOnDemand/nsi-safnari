package object support {
  implicit class AnyOps[A](a: A) {
    def tap(f: A => Unit): A = { f(a); a }
  }
}
