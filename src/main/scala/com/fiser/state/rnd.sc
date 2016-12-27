import com.fiser.state.{RNG, SimpleRNG}

import scala.annotation.tailrec

val rng = SimpleRNG(42)
rng.nextInt
rng.nextInt

val (n1, rng2) = rng.nextInt
val (n2, rng3) = rng2.nextInt

def randomPair(rng: RNG): ((Int, Int), RNG) = {
  val (n1, rng1) = rng.nextInt
  val (n2, rng2) = rng1.nextInt
  ((n1, n2), rng2)
}

randomPair(rng)

//6.2
-(Int.MinValue + 1)
def nonNegativeInt(rng: RNG): (Int, RNG) = {
  val (n, nextRNG) = rng.nextInt
  if (n < 0)
    (-(n + 1), nextRNG)
  else
    (n, nextRNG)
}
val (n3, rng4) = nonNegativeInt(rng3)

//6.3
def double(rng: RNG): (Double, RNG) = {
  val (nr, nextRNG) = nonNegativeInt(rng)
  (nr.toDouble / Int.MaxValue, nextRNG)
}

double(rng2)

def intDouble(rng: RNG): ((Int, Double), RNG) = {
  val (i, nextRNG) = rng.nextInt
  val (d, nextNextRNG) = double(nextRNG)
  ((i, d), nextNextRNG)
}

intDouble(rng3)

//6.4
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

type Rand[+A] = RNG => (A, RNG)

val randInt: Rand[Int] = _.nextInt

def unit[A](a: A): Rand[A] = rnd => (a, rnd)

def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
  rng =>
    val (a, rng2) = s(rng)
    (f(a), rng2)
}


map(nonNegativeInt)(x => x + 1)(rng)

def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

//6.5
def doubleWithMap: Rand[Double] = {
  map(nonNegativeInt)(i => i.toDouble / Int.MaxValue)
}
doubleWithMap(rng)

//6.6
def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
  rnd => {
    val (a, rndA) = ra(rnd)
    val (b, rndB) = rb(rndA)
    (f(a, b), rndB)
  }
}

def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = {
  map2(ra, rb)((_, _))
}

def randIntDouble: Rand[(Int, Double)] =
  both(nonNegativeInt, double)

val rndIntDouble = randIntDouble(rng)

//6.7
def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
  fs.foldRight(unit(List.empty[A])) {
    (f, acc) => map2(f, acc)((a: A, b: List[A]) => a :: b)
  }
}
val list = List.fill[Rand[Int]](10)(rng => rng.nextInt)
sequence(list)(rng)

def nonNegativeLessThen(n: Int): Rand[Int] =
  map(nonNegativeInt)(_ % n)

nonNegativeLessThen(7)(rng)

//6.8
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
    a =>
      map_2(rb) {
        b => f(a, b)
      }
  }
}

def rollDie = map(nonNegativeLessThen(6))(_ + 1)
rollDie(SimpleRNG(5))