def fib(n: Int): Int =
  @annotation.tailrec
  def go(n: Int, current: Int, next: Int): Int =
    if n <= 0 then
      current
    else
      go(n - 1, next, current + next)

  go(n, 0, 1)

(1 to 10).map(fib)

def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean =
  @annotation.tailrec
  def go(n: Int): Boolean =
    if n + 1 >= as.size then
      true
    else if gt(as(n), as(n + 1)) then
      false
    else
      go(n + 1)

  go(0)

isSorted(Array(1, 2, 3), _ > _)
isSorted(Array(1, 2, 1), _ > _)
isSorted(Array(3, 2, 1), _ < _)
isSorted(Array(3, 2, 3), _ < _)

def curry[A, B, C](f: (A, B) => C): A => B => C =
  a => f(a, _)

def foo(a: Int, b: Int): Int = ???

val curried = curry(foo)

def uncurry[A, B, C](f: A => B => C): (A, B) => C =
  (a, b) => f(a)(b)

uncurry(curried)

def compose[A, B, C](f: B => C, g: A => B): A => C =
  a => f(g(a))

def f(b: String): Boolean = ???
def g(a: Int): String = ???

compose(f, g)
