trait RNG:
  def nextInt: (Int, RNG)
case class SimpleRNG(seed: Long) extends RNG:
  def nextInt: (Int, RNG) =
    val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)

def nonNegativeInt(rng: RNG): (Int, RNG) =
  val (value, next) = rng.nextInt
  val nonNegative = if value == Int.MinValue then 0 else value.abs
  nonNegative -> next

def int: Rand[Int] =
  _.nextInt

def double: Rand[Double] =
  nonNegativeInt.map: value =>
    value.toDouble / Int.MaxValue

def intDouble: Rand[(Int, Double)] =
  Rand.both(int, double)

def doubleInt: Rand[(Double, Int)] =
  Rand.both(double, int)

def double3: Rand[(Double, Double, Double)] =
  Rand.sequence(List.fill(3)(double)).map:
    case a :: b :: c :: Nil =>
      (a, b, c)

def ints(count: Int)(rng: RNG): (List[Int], RNG) =
  @annotation.tailrec
  def loop(acc: List[Int])(count: Int, rng: RNG): (List[Int], RNG) =
    if count == 0 then
      acc -> rng
    else
      val (value, next) = rng.nextInt
      loop(value :: acc)(count - 1, next)

  loop(Nil)(count, rng)

val seed = 42

nonNegativeInt(SimpleRNG(seed))
double(SimpleRNG(seed))

intDouble(SimpleRNG(seed))
doubleInt(SimpleRNG(seed))
double3(SimpleRNG(seed))

ints(5)(SimpleRNG(seed))

type Rand[+A] = State[RNG, A]

object Rand:

  def unit[A](a: A): Rand[A] =
    rng =>
      a -> rng

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    State.map2(ra, rb)(f)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)(_ -> _)

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    State.sequence(rs)

def nonNegativeEven: Rand[Int] =
  nonNegativeInt.map: value =>
    value - (value % 2)

def nonNegativeLessThan(n: Int): Rand[Int] =
  nonNegativeInt.flatMap: value =>
    val mod = value % n
    if value + (n - 1) - mod >= 0 then
      Rand.unit(mod)
    else
      nonNegativeLessThan(n)

nonNegativeEven(SimpleRNG(seed))
nonNegativeLessThan(100)(SimpleRNG(seed))

opaque type State[S, +A] = S => (A, S)

extension [S, A](self: State[S, A])

  def run(s: S): (A, S) =
    self(s)

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    state =>
      val (a, state_) = self.run(state)
      f(a)(state_)

  def map[B](f: A => B): State[S, B] =
    flatMap: a =>
      State.unit(f(a))

  def get: State[S, S] =
    s =>
      s -> s

  def set(s: S): State[S, Unit] =
    _ =>
      () -> s

  def modify(f: S => S): State[S, Unit] =
    for
      s <- get
      _ <- set(f(s))
    yield ()

object State:

  def apply[S, A](f: S => (A, S)): State[S, A] =
    f

  def unit[S, A](a: A): State[S, A] =
    a -> _

  def map2[S, A, B, C](
      sa: State[S, A],
      sb: State[S, B],
  )(f: (A, B) => C): State[S, C] =
    for
      a <- sa
      b <- sb
    yield f(a, b)

  def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] =
    ss.foldRight(unit(List.empty[A])): (sa, acc) =>
      map2(sa, acc)(_ :: _)

enum Input:
  case Coin
  case Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine:
  def simulate(inputs: List[Input]): State[Machine, (Int, Int)] =
    val simulation = State.sequence(inputs.map { input =>
      State[Machine, (Int, Int)] { state =>
        input match
          case Input.Coin if state.locked && state.candies > 0 =>
            val coins = state.coins + 1
            val state_ = state.copy(locked = false, coins = coins)
            (coins, state.candies) -> state_
          case Input.Turn if !state.locked =>
            val candies = state.candies - 1
            val state_ = state.copy(locked = true, candies = candies)
            (state.coins, candies) -> state_
          case _ =>
            (state.coins, state.candies) -> state
      }
    })
    simulation.map(_.last)

val ((coins, candies), _) =
  Machine.simulate(List(Input.Coin, Input.Turn)).run(Machine(
    locked = true,
    candies = 5,
    coins = 10,
  ))
