package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](t: Tree[A]): Int = fold(t)(a => 1)(1 + _ + _)

  def depth[A](t: Tree[A]): Int = fold(t)(a => 0)((d1,d2) => 1 + d1 max d2)

  def map[A,B](t: Tree[A])(f: A => B) = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(x) => f(x)
    case Branch(l,r) => g(fold(l)(f)(g),fold(r)(f)(g))
  }

}