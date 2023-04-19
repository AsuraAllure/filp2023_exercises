package writer

import typeclasses._

case class Writer[Log, A](log: Log, value: A) {
  def tell(nextLog: Log)(implicit semigroup: Semigroup[Log]): Writer[Log, A] =
    Writer(semigroup.combine(log, nextLog), value)
}

object Writer {
  implicit def monad[Log: Monoid]: Monad[Writer[Log, *]] = new Monad[Writer[Log, *]] {
    def pure[A](value: A): Writer[Log, A] =
      Writer(Monoid[Log].empty, value)

    def flatMap[A, B](fa: Writer[Log, A])(f: A => Writer[Log, B]): Writer[Log, B] = {
      val fb = f(fa.value)
      Writer(Semigroup[Log].combine(fa.log, fb.log), fb.value)
    }

    def map[A, B](fa: Writer[Log, A])(f: A => B): Writer[Log, B] =
      flatMap(fa)(a => pure(f(a)))
  }

  def tell[Log](log: Log): Writer[Log, Unit] =
    Writer(log, ())
}
