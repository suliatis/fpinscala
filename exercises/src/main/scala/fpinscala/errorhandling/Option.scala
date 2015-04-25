package fpinscala.errorhandling

import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(v) => Some(f(v))
    case _ => None
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case Some(v) => v
    case _ => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def orElse[B>:A](ob: => Option[B]): Option[B] = map(Some(_)) getOrElse ob

  def filter(p: A => Boolean): Option[A] = this match {
    case Some(v) if p(v) => this
    case _ => None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  //a flatMap (aa => b map (bb => f(aa, bb)))
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = for (aa <- a; bb <- b) yield f(aa, bb)

  def sequence[A](as: List[Option[A]]): Option[List[A]] = traverse(as)(a => a)

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight[Option[List[B]]](Some(Nil))((x, xs) => map2(f(x), xs)(_ :: _))
}