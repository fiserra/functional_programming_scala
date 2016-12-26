import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

val rng = SimpleRNG(42)

val (n1, rng2) = rng.nextInt
val (n2, rng3) = rng2.nextInt

def nonNegativeInt(rng: RNG): (Int, RNG) = {
  val (n, nextRNG) = rng.nextInt
  if (n < 0)
    (-(n + 1), nextRNG)
  else
    (n, nextRNG)
}
val (n3, rng4) = nonNegativeInt(rng3)

def double(rng: RNG): (Double, RNG) = {
  val (nr, nextRNG) = nonNegativeInt(rng)
  (nr / (Int.MaxValue.toDouble + 1), nextRNG)

}

double(rng)

def intDouble(rng: RNG): ((Int, Double), RNG) = {
  val (i, nextRNG) = rng.nextInt
  val (d, nextNextRNG) = double(nextRNG)
  ((i, d), nextNextRNG)
}

intDouble(rng)

def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
  @tailrec
  def innerInts(cnt: Int, acc: List[Int])(rng: RNG): (List[Int], RNG) = {
    val (i, nextRNG) = rng.nextInt
    if (cnt == 1) (i :: acc, nextRNG)
    else innerInts(cnt - 1, i :: acc)(nextRNG)
  }
  innerInts(count, Nil)(rng)
}

ints(2)(rng)


type State[S, +A] = S => (A, S)

type Rand[A] = RNG => (A, RNG)

def unit[A](a: A): Rand[A] = rnd => (a, rnd)

def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
  rng =>
    val (a, rng2) = s(rng)
    (f(a), rng2)
}

def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

def doubleMap: Rand[Double] = {
  map(nonNegativeInt)(i => i / (Int.MaxValue + 1).toDouble)
}

doubleMap(rng)

def combine[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
  rnd => {
    val (a, rndA) = ra(rnd)
    val (b, rndB) = rb(rndA)
    (f(a, b), rndB)
  }
}

def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = {
  combine(ra, rb)((_, _))
}

def randIntDouble: Rand[(Int, Double)] =
  both(nonNegativeInt, double)

randIntDouble(rng)

def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
  rnd => {
    val result = fs.foldLeft((rnd, List.empty[A])) {
      (z, ra) => {
        val (a, r) = ra(z._1)
        (r, a :: z._2)
      }
    }
    (result._2, result._1)
  }
}

def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] = {
  fs.foldRight(unit(List.empty[A])) {
    (f, acc) => combine(f, acc)((a: A, b: List[A]) => a :: b)

  }
}

sequence(List.fill[Rand[Int]](10)(rng => rng.nextInt))(rng)
sequence2(List.fill[Rand[Int]](10)(rng => rng.nextInt))(rng)

def nonNegativeLessThen(n: Int): Rand[Int] =
  map(nonNegativeInt)(_ % n)

nonNegativeLessThen(7)(rng)

def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
  rng => {
    val (a, rnga) = f(rng)
    g(a)(rnga)
  }


def map_2[A, B](s: Rand[A])(f: A => B): Rand[B] = {
  flatMap(s)(a => unit(f(a)))
}

def combine_2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
  flatMap(ra) {
    a => map_2(rb) {
      b => f(a, b)
    }
  }
}


