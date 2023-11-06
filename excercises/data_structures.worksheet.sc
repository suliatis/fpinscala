import scala.annotation.tailrec

enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])

object List:

  def apply[A](as: A*): List[A] =
    if as.isEmpty then
      Nil
    else
      Cons(as.head, apply(as.tail*))

  def sum(ints: List[Int]): Int =
    foldLeft(ints, 0, _ + _)

  def incrementEach(ints: List[Int]): List[Int] =
    map(ints, _ + 1)

  def addPairwise(left: List[Int], right: List[Int]): List[Int] =
    zipWith(left, right, _ + _)

  def product(doubles: List[Double]): Double =
    foldLeft(doubles, 1.0, _ * _)

  def tail[A](as: List[A]): List[A] =
    as match
      case Nil =>
        sys.error("tail of empty list")
      case Cons(_, tl) =>
        tl

  def setHead[A](as: List[A], head: A): List[A] =
    as match
      case Nil =>
        sys.error("set head of empty list")
      case Cons(_, tail) =>
        Cons(head, tail)

  def drop[A](as: List[A], n: Int): List[A] =
    as match
      case Nil =>
        Nil
      case Cons(_, tail) if n >= 1 =>
        drop(tail, n - 1)
      case _ =>
        as

  def dropWhile[A](as: List[A], p: A => Boolean): List[A] =
    as match
      case Nil =>
        Nil
      case Cons(head, tail) if p(head) =>
        dropWhile(tail, p)
      case _ =>
        as

  def append[A](as1: List[A], as2: List[A]): List[A] =
    foldRight(as1, as2, Cons(_, _))

  def init[A](as: List[A]): List[A] =
    as match
      case Nil | Cons(_, Nil) =>
        Nil
      case Cons(head, tail) =>
        Cons(head, init(tail))

  def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(reverse(as), acc, (a, b) => f(b, a))

  def size[A](as: List[A]): Int =
    foldRight(as, 0, (_, size) => size + 1)

  @tailrec
  def foldLeft[A, B](as: List[A], acc: B, f: (B, A) => B): B =
    as match
      case Nil =>
        acc
      case Cons(head, tail) =>
        foldLeft(tail, f(acc, head), f)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A], (acc, a) => Cons(a, acc))

  def flatten[A](aas: List[List[A]]): List[A] =
    foldRight(aas, Nil: List[A], append)

  def map[A, B](as: List[A], f: A => B): List[B] =
    foldRight(as, Nil: List[B], (a, acc) => Cons(f(a), acc))

  def filter[A](as: List[A], p: A => Boolean): List[A] =
    flatMap(as, a => if p(a) then Cons(a, Nil) else Nil)

  def flatMap[A, B](as: List[A], f: A => List[B]): List[B] =
    flatten(map(as, f))

  def zipWith[A, B, C](as: List[A], bs: List[B], f: (A, B) => C): List[C] =

    @tailrec
    def go(_as: List[A], _bs: List[B], cs: List[C]): List[C] =
      (_as, _bs) match
        case (Nil, _) | (_, Nil) =>
          cs
        case (Cons(h1, t1), Cons(h2, t2)) =>
          go(t1, t2, Cons(f(h1, h2), cs))

    go(as, bs, Nil)
  end zipWith

  @tailrec
  def startsWith[A](sup: List[A], sub: List[A]): Boolean =
    (sup, sub) match
      case (Cons(h1, t1), Cons(h2, t2)) =>
        h1 == h2 && startsWith(t1, t2)
      case (_, Nil) =>
        true
      case (_, _) =>
        false

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    (sup, sub) match
      case (Cons(_, _sup), Cons(_, _)) =>
        startsWith(sup, sub) || hasSubsequence(_sup, sub)
      case (_, Nil) =>
        true
      case (_, _) =>
        false

import List.*

sum(List(1, 2, 3, 4, 5))
product(List(1, 2, 3, 4, 5))
tail(List(1, 2, 3, 4, 5))
setHead(List(1, 2, 3, 4, 5), 6)
drop(List(1, 2, 3, 4, 5), 3)
dropWhile(List(1, 2, 3, 4, 5), _ <= 3)
append(List(1, 2, 3), List(4, 5))
init(List(1, 2, 3, 4, 5))
size(List(1, 2, 3, 4, 5))
reverse(List(1, 2, 3, 4, 5))
flatten(List(List(1, 2), Nil, List(3), List(4, 5)))
incrementEach(List(1, 2, 3, 4, 5))
filter(List(1, 2, 3, 4, 5), _ % 2 == 0)
addPairwise(List(1, 2, 3), List(4, 5, 6))

hasSubsequence(Nil, Nil)
hasSubsequence(Nil, List(1, 2))
hasSubsequence(List(1, 2, 3, 4, 5), Nil)
hasSubsequence(List(1, 2, 3, 4, 5), List(1))
hasSubsequence(List(1, 2, 3, 4, 5), List(3))
hasSubsequence(List(1, 2, 3, 4, 5), List(5))
hasSubsequence(List(1, 2, 3, 4, 5), List(1, 2))
hasSubsequence(List(1, 2, 3, 4, 5), List(3, 4))
hasSubsequence(List(1, 2, 3, 4, 5), List(3, 5))

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def fold[B](f: A => B)(g: (B, B) => B): B =
    this match
      case Leaf(value) =>
        f(value)
      case Branch(left, right) =>
        g(left.fold(f)(g), right.fold(f)(g))

  def size(): Int =
    fold(_ => 1)(1 + _ + _)

  def depth(): Int =
    fold(_ => 1): (d1, d2) =>
      1 + (d1 max d2)

  def map[B](f: A => B): Tree[B] =
    fold(a => Leaf(f(a))): (l, r) =>
      Branch(l, r)

extension (self: Tree[Int])

  def maximum(): Int =
    self match
      case Tree.Leaf(value) =>
        value
      case Tree.Branch(left, right) =>
        left.maximum() max right.maximum()

import Tree.*

Branch(Leaf(1), Branch(Leaf(2), Leaf(3))).size()
Branch(Leaf(1), Branch(Leaf(2), Leaf(3))).maximum()
Branch(Leaf(1), Branch(Leaf(2), Leaf(3))).depth()
Branch(Leaf(1), Branch(Leaf(2), Leaf(3))).map(_ + 1)
