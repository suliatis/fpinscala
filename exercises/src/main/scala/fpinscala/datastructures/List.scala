package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def append[A](left: List[A], right: List[A]): List[A] = foldRight(left, right)(Cons(_, _))

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((b, a) => f(a, b))// Utility functions

  def foldSum(ns: List[Int]) = foldRight(ns, 0)(_ + _)

  def foldProduct(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = Cons(h, tail(l))

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }

  def dropWhile[A](l: List[A], p: A => Boolean): List[A] = l match {
    case Cons(h, t) if p(h) => dropWhile(t, p)
    case _ => l
  }

  //not the most effective way to get the init of a list
  def init[A](l: List[A]): List[A] = reverse(tail(reverse(l)))

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil:List[A])((t, h) => Cons(h, t))

  def flatMap[A,B](l: List[A])(f: A => List[B]) = foldRight(l, Nil:List[B])((x, xs) => append(f(x), xs))

  def map[A,B](l: List[A])(f: A => B): List[B] = flatMap(l)(x => Cons(f(x), Nil))

  def filter[A](l: List[A])(p: A => Boolean): List[A] = flatMap(l)(x => if (p(x)) Cons(x, Nil) else Nil)
}
