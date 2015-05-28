package fpinscala.state

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  import State._

  val int: Rand[Int] = State(s => s.nextInt)

  val nonNegativeInt: Rand[Int] = int.map(i => if (i < 0) -(i + 1) else i)

  val nonNegativeEvenInt: Rand[Int] = nonNegativeInt.map(i => i - i % 2)

  val double: Rand[Double] = nonNegativeInt.map(_ / (Int.MaxValue.toDouble + 1))

  val intDouble: Rand[(Int,Double)] = both(int, double)

  val doubleInt: Rand[(Double,Int)] = both(double,int)

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = ra.map2(rb)((_,_))

  def nonNegativeLessThan(n: Int): Rand[Int] = nonNegativeInt.flatMap { i =>
    val mod = i % n
    if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  }
}

import State._

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =  for {
    a <- this
    b <- sb
  } yield f(a,b)

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, sa) = run(s)
    f(a).run(sa)
  })
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S,A](a: A): State[S,A] = State(s => (a, s))

  def sequence[S,A](sas: List[State[S,A]]): State[S,List[A]] = sas.foldRight(unit[S,List[A]](Nil))((a, acc) => a.map2(acc)(_ :: _))

  def get[S]: State[S,S] = State(s => (s,s))

  def set[S](s: S): State[S,Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S,Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence { inputs.map { i =>
        modify { s: Machine =>
          (i, s) match {
            case (Turn, Machine(true, _, _)) => s
            case (Coin, Machine(false, _, _)) => s
            case (_, Machine(_, _, 0)) => s
            case (Coin, Machine(true, coin, candy)) => Machine(false, coin + 1, candy)
            case (Turn, Machine(false, coin, candy)) => Machine(true, coin, candy - 1)
          }
        }
      }
    }
    s <- get
  } yield (s.coins, s.candies)

  def main (args: Array[String]) {
    println(simulateMachine(List.fill(4)(List(Coin,Turn,Turn)).flatten).run(Machine(true, 10, 5)))
  }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

