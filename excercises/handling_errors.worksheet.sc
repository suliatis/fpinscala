enum Option[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): Option[B] =
    this match
      case Some(get) =>
        Some(f(get))
      case None =>
        None

  def getOrElse[B >: A](default: => B): B =
    this match
      case Some(get) =>
        get
      case None =>
        default

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def orElse[B >: A](default: => Option[B]): Option[B] =
    map(Some(_)).getOrElse(default)

  def filter(p: A => Boolean): Option[A] =
    flatMap: a =>
      if p(a) then
        Some(a)
      else
        None

import Option.*

case class Employee(
    name: String,
    department: String,
    manager: Option[Employee],
)

object Employee:

  def lookupByName(name: String): Option[Employee] =
    if name == "John" then
      Some(Employee(
        "John",
        "John's Department",
        Some(Employee("Jane", "Boss", None)),
      ))
    else
      None

Employee.lookupByName("John").map(_.department)
Employee.lookupByName("John").flatMap(_.manager)
Employee.lookupByName("Joe").map(_.department).getOrElse("Default Dept.")

def mean(xs: Seq[Double]): Option[Double] =
  if xs.isEmpty then
    None
  else
    Some(xs.sum / xs.length)

def variance(xs: Seq[Double]): Option[Double] =
  mean(xs).flatMap: m =>
    val xs_ = xs.map: x =>
      math.pow(x - m, 2)
    mean(xs_)

def map2[A, B, C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] =
  (oa, ob) match
    case (Some(a), Some(b)) =>
      Some(f(a, b))
    case (_, _) =>
      None

def sequence[A](as: List[Option[A]]): Option[List[A]] =
  traverse(as)(Some(_))

def traverse[A, B](as: List[Option[A]])(f: A => Option[B]): Option[List[B]] =
  val init: Option[List[B]] = Some(Nil)
  as.foldRight(init): (a, acc) =>
    val b = a.flatMap(f)
    map2(b, acc):
      _ :: _

enum Either[+E, +A]:
  case Left(value: E)
  case Right(value: A)

  def map[B](f: A => B): Either[E, B] =
    this match
      case Right(a) =>
        Right(f(a))
      case Left(e) =>
        Left(e)

  def flatMap[E_ >: E, B](f: A => Either[E_, B]): Either[E_, B] =
    this match
      case Right(a) =>
        f(a)
      case Left(e) =>
        Left(e)

  def orElse[E_ >: E, A_ >: A](default: => Either[E_, A_]): Either[E_, A_] =
    this match
      case Right(a) =>
        this
      case Left(_) =>
        default

  def map2[E_ >: E, B, C](that: Either[E_, B])(f: (A, B) => C): Either[E_, C] =
    for
      a <- this
      b <- that
    yield f(a, b)

object Either:

  import scala.util.control.NonFatal
  def catchNonFatal[A](a: => A): Either[Throwable, A] =
    try
      Right(a)
    catch
      case NonFatal(t) =>
        Left(t)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    val init: Either[E, List[B]] = Right(Nil)
    as.foldRight(init): (a, acc) =>
      val b = f(a)
      b.map2(acc):
        _ :: _

  def sequence[E, A](as: List[Either[E, A]]): Either[E, List[A]] =
    traverse(as)(identity)

def safeDiv(x: Int, y: Int): Either[Throwable, Int] =
  Either.catchNonFatal(x / y)

def parseInsuranceRateQuote(
    age: String,
    numberOfSpeedingTickets: String,
): Either[Throwable, Double] =
  for
    a <- Either.catchNonFatal(age.toInt)
    tickets <- Either.catchNonFatal(numberOfSpeedingTickets.toInt)
  yield ???
