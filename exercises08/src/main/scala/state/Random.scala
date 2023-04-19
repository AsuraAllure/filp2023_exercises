package state

import typeclasses.Monad

object StatefulRandom {

  import scala.util.Random

  // Если вам необходимо сгенерить рандомное число, то вы можете использовать класс scala.util.Random.
  println(Random.nextDouble)
  println(Random.nextInt)
  println(Random.nextInt)

  // Каждый вызов возвращает новое число. Можно предположить, что rnd имеет какое-то внутреннее
  // состояние. Когда происходит очередной вызов nextInt/nextDouble, то rnd не только возвращает
  // новое случайное число, но изменяет внутреннее состояние.
  //
  // Такое скрытое изменение внутреннего состояние имеет ряд недостатков: программы тяжелее
  // композировать, тестировать, усложняется написание многопоточных программ.
}

object StatelessRandom {

  // Чтобы избавиться от скрытого изменяемого состояния, давайте сделаем его явным. Новая функция
  // nextInt возвращает не только случайное число, но и новое состояние генератора случайных чисел.
  trait Random {
    def nextInt: (Int, Random)
  }

  // В качесте простой реализации генератора случайных чисел будем использовать следующий класс
  case class SimpleRandom(seed: Long) extends Random {
    override def nextInt: (Int, Random) = {
      val newSeed = 13 * seed + 41
      val int     = (newSeed >>> 3).toInt
      (int, SimpleRandom(newSeed))
    }
  }

  // и простой пример использования
  val rnd: SimpleRandom     = SimpleRandom(12)
  val (firstInt, nextRnd1)  = rnd.nextInt
  val (secondInt, nextRnd2) = nextRnd1.nextInt
  val (thirdInt, nextRnd3)  = nextRnd2.nextInt
  // Вместо скрытого изменения состояния, мы теперь делаем все изменения явными. Чтобы получать
  // каждый раз разные значения, мы вызываем nextInt на разных объектах: rnd, nextRnd1, nextRnd2 и
  // так далее.

  // Реализуйте функцию, которая будет генерировать пару случайных целых чисел
  def pair(rnd: Random): ((Int, Int), Random) = {
    val (firstInt, nextRnd1)  = rnd.nextInt
    val (secondInt, nextRnd2) = nextRnd1.nextInt
    ((firstInt, secondInt), nextRnd2)
  }

  // Функцию, которая генерирует неотрицальные числа
  def nonNegativeInt(rnd: Random): (Int, Random) = {
    val (nextInt, nextRnd) = rnd.nextInt
    (math.abs(nextInt), nextRnd)
  }

  // Функцию, которая генерирует случайное число от нуля включительно до единицы невключительно
  def double(rnd: Random): (Double, Random) = {
    val (i, nextRnd) = nonNegativeInt(rnd)
    (i / (Int.MaxValue.toDouble + 1), nextRnd)
  }
}

// Нам удалось избавиться от скрытого изменяемого состояния, но приходится передавать теперь его явно.
// Это приводит к написанию достаточно однообразного кода. Давайте попробуем немного модифицировать
// подход, чтобы избавиться от этого.
object BetterStatelessRandom {

  import StatelessRandom.Random

  // Можно заметить, что наши функции pair, nonNegativeInt, double имеют одинаковый шаблон
  // Rnd => (A, Rnd), где тип A зависит от конкретной функции. Можно сказать, что эти функции
  // описывают переход состояния. Давайте обобщим этот шаблон в виде класса RandomState.
  case class RandomState[A](run: Random => (A, Random)) {
    def map[B](f: A => B): RandomState[B] =
      RandomState { rnd =>
        val (a, nextRnd) = run(rnd)
        (f(a), nextRnd)
      }

    def flatMap[B](f: A => RandomState[B]): RandomState[B] =
      RandomState { rnd =>
        val (a, nextRnd) = run(rnd)
        f(a).run(nextRnd)
      }
  }

  object RandomState {
    implicit val monad: Monad[RandomState] = new Monad[RandomState] {
      def pure[A](x: A): RandomState[A] =
        RandomState(rnd => (x, rnd))

      def flatMap[A, B](fa: RandomState[A])(f: A => RandomState[B]): RandomState[B] =
        fa.flatMap(f)

      def map[A, B](fa: RandomState[A])(f: A => B): RandomState[B] =
        fa.map(f)
    }

    def unit[A](a: A): RandomState[A] =
      RandomState(rng => (a, rng))
  }

  // Теперь класс RandomState может быть использован внутри for comprehension

  // Функция возвращает случайное целое число
  val nextInt: RandomState[Int] =
    RandomState(rng => rng.nextInt)

  // Функция возвращает случайное неотрицальное целое число
  val nonNegativeInt: RandomState[Int] =
    nextInt.map(_.abs)

  // Функция возвращает пару случайных неотрицальных целых чисел
  val pair: RandomState[(Int, Int)] =
    for {
      x <- nextInt
      y <- nextInt
    } yield (x, y)

  // Функция возвращает случайное число от нуля до единицы
  val double: RandomState[Double] =
    nonNegativeInt.map(_ / (Int.MaxValue.toDouble + 1))

  // Функция возвращает список случайной длины из случайных целых чисел
  val randomList: RandomState[List[Int]] =
    for {
      x  <- nextInt
      xs <- sequence(List.fill(x)(nextInt))
    } yield xs

  // Функция должна сконвертировать список из случайных состояний в случайное состояние, которое
  // возвращает список.
  def sequence[A](xs: List[RandomState[A]]): RandomState[List[A]] =
    xs.foldLeft(RandomState.unit(List.empty[A])) { (acc, elem) =>
        elem.flatMap(x => acc.map(xs => x :: xs))
      }
      .map(_.reverse)
}
