import annotation.tailrec
import LazyList.empty

enum LazyList[+A]:
  case Empty
  case Cons(head: () => A, tail: () => LazyList[A])

  import LazyList.empty
  import LazyList.cons
  import LazyList.unfold

  def toList(): List[A] =

    @tailrec
    def go(acc: List[A], rest: LazyList[A]): List[A] =
      rest match
        case Empty =>
          acc.reverse
        case Cons(head, tail) =>
          go(head() :: acc, tail())

    go(Nil, this)

  def take(n: Int): LazyList[A] =
    unfold(n -> this): (n_, rest) =>
      rest match
        case Cons(head, tail) if n_ >= 1 =>
          Some(head() -> ((n_ - 1) -> tail()))
        case _ =>
          None

  def takeWhile(p: A => Boolean): LazyList[A] =
    unfold(this): rest =>
      rest match
        case Cons(head, tail) if p(head()) =>
          Some(head() -> tail())
        case _ =>
          None

  def drop(n: Int): LazyList[A] =
    this match
      case Cons(_, tail) if n > 1 =>
        tail().drop(n - 1)
      case Cons(_, tail) if n == 1 =>
        tail()
      case _ =>
        empty()

  def exists(p: A => Boolean): Boolean =
    foldRight(false): (a, assumption) =>
      p(a) || assumption

  def forAll(p: A => Boolean): Boolean =
    foldRight(true): (a, assumption) =>
      p(a) && assumption

  def headOption(): Option[A] =
    foldRight(None: Option[A]): (a, _) =>
      Some(a)

  def map[B](f: A => B): LazyList[B] =
    foldRight(empty[B]()): (a, acc) =>
      cons(f(a), acc)

  def mapViaUnfold[B](f: A => B): LazyList[B] =
    unfold(this): rest =>
      rest match
        case Cons(head, tail) =>
          Some(f(head()), tail())
        case _ =>
          None

  def filter(f: A => Boolean): LazyList[A] =
    foldRight(empty[A]()): (a, acc) =>
      if f(a) then
        cons(a, acc)
      else
        acc

  def append[A_ >: A](that: => LazyList[A_]): LazyList[A_] =
    foldRight(that): (a, acc) =>
      cons(a, acc)

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(empty[B]()): (a, acc) =>
      f(a).append(acc)

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption()

  def foldRight[B](acc: => B)(f: (A, => B) => B): B =
    this match
      case Cons(head, tail) =>
        f(head(), tail().foldRight(acc)(f))
      case _ =>
        acc

  def startsWith[A](prefix: LazyList[A]): Boolean =
    zipAll(prefix)
      .takeWhile: (_, a2) =>
        a2.isDefined
      .forAll: (a1, a2) =>
        a1 == a2

  def tails(): LazyList[LazyList[A]] =
    scanRight[LazyList[A]](empty()): (as, acc) =>
      cons(as, acc)

  def scanRight[B](acc: => B)(f: (A, => B) => B): LazyList[B] =
    unfold(Some(this): Option[LazyList[A]]):
      case Some(current @ Cons(_, tail)) =>
        Some(current.foldRight(acc)(f) -> Some(tail()))
      case Some(Empty) =>
        Some(acc -> None)
      case None =>
        None

  def hasSubsequence[A](other: LazyList[A]): Boolean =
    tails().exists(_ == other)

  def zipWith[B, C](that: LazyList[B], f: (A, B) => C): LazyList[C] =
    unfold(this -> that):
      case Cons(a, as) -> Cons(b, bs) =>
        Some(f(a(), b()) -> (as() -> bs()))
      case _ =>
        None

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    unfold(this -> that):
      case Cons(a, as) -> Cons(b, bs) =>
        Some(Some(a()) -> Some(b()) -> (as() -> bs()))
      case Cons(a, as) -> Empty =>
        Some(Some(a()) -> None -> (as() -> empty()))
      case Empty -> Cons(b, bs) =>
        Some(None -> Some(b()) -> (empty() -> bs()))
      case Empty -> Empty =>
        None

object LazyList:

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then
      empty()
    else
      cons(as.head, apply(as.tail*))

  def cons[A](head: => A, tail: => LazyList[A]): LazyList[A] =
    lazy val head_ = head
    lazy val tail_ = tail
    Cons(() => head_, () => tail_)

  def empty[A](): LazyList[A] =
    Empty

  val ones: LazyList[Int] =
    continually(1)

  def continually[A](a: A): LazyList[A] =
    unfold(()): _ =>
      Some(a -> ())

  def from(n: Int): LazyList[Int] =
    unfold(n): current =>
      Some(current -> (current + 1))

  def fibs(): LazyList[Int] =
    unfold(0 -> 1): (current, next) =>
      Some(current -> (next -> (current + next)))

  def unfold[A, S](init: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(init) match
      case None =>
        empty()
      case Some(a -> state) =>
        cons(a, unfold(state)(f))

val xs = LazyList(1, 2, 3, 4, 5)

xs.toList() == List(1, 2, 3, 4, 5)

xs.take(2).toList() == xs.toList().take(2)
xs.take(7).toList() == xs.toList()
empty[Int]().take(2).toList() == Nil

xs.drop(2).toList() == xs.toList().drop(2)
xs.drop(7).toList() == Nil
empty[Int]().drop(2).toList() == Nil

xs.takeWhile(_ < 3).toList() == xs.toList().takeWhile(_ < 3)
xs.takeWhile(_ < 7).toList() == xs.toList()
empty[Int]().takeWhile(_ < 3).toList() == Nil

xs.exists(_ == 3) == xs.toList().exists(_ == 3)
xs.exists(_ == 7) == xs.toList().exists(_ == 7)
empty[Int]().exists(_ == 3) == false

xs.forAll(_ < 3) == xs.toList().forall(_ < 3)
xs.forAll(_ < 7) == xs.toList().forall(_ < 7)
empty[Int]().forAll(_ < 3) == true

xs.headOption() == xs.toList().headOption
empty[Int]().headOption() == None

xs.map(_ + 1).toList() == xs.toList().map(_ + 1)
empty[Int]().map(_ + 1).toList() == Nil

xs.mapViaUnfold(_ + 1).toList() == xs.toList().map(_ + 1)
empty[Int]().mapViaUnfold(_ + 1).toList() == Nil

xs.filter(_ % 2 == 0).toList() == xs.toList().filter(_ % 2 == 0)
empty[Int]().filter(_ % 2 == 0).toList() == Nil

xs.append(xs).toList() == xs.toList() ++ xs.toList()
empty[Int]().append(xs).toList() == xs.toList()

xs.flatMap(x => LazyList(x, x)).toList() == xs.toList().flatMap(x => List(x, x))
empty[Int]().flatMap(x => LazyList(x, x)).toList() == Nil

xs.find(_ == 3) == xs.toList().find(_ == 3)
xs.find(_ == 7) == None
empty[Int]().find(_ == 3) == None

xs.startsWith(xs) == true
xs.startsWith(xs.take(3)) == true
xs.take(3).startsWith(xs) == false
xs.startsWith(empty()) == true

xs.tails().map(_.toList()).toList() == List(
  List(1, 2, 3, 4, 5),
  List(2, 3, 4, 5),
  List(3, 4, 5),
  List(4, 5),
  List(5),
  Nil
)

xs.zipWith(xs, _ + _).toList() == xs.toList().zip(xs.toList()).map(_ + _)
empty[Int]().zipWith(xs, _ + _).toList() == Nil

LazyList.continually(1).take(5).toList() == List(1, 1, 1, 1, 1)

LazyList.ones.take(5).toList() == List(1, 1, 1, 1, 1)

LazyList.from(1).take(5).toList() == List(1, 2, 3, 4, 5)

LazyList.fibs().take(10).toList() == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
