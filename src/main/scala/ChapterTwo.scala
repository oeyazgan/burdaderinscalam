object ChapterTwo:
  def myAbs(n: Int): Int =
    if n > 0 then n
    else -n

  @annotation.tailrec
  private def factorial(n: Int, res: Int): Int =
    if n < 2 then res
    else factorial(n - 1, n * res)

  def findFirstPolymorphic[A](arr: Array[A], key: A): Int =
    @annotation.tailrec
    def loop(n: Int): Int =
      if n >= arr.length then -1
      else if arr(n) == key then n
      else loop(n + 1)
    loop(0)

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean =
    @annotation.tailrec
    def f(n: Int, prevVal: A): Boolean =
      if n >= as.length then true
      else if gt(as(n), prevVal) then f(n + 1, as(n))
      else false

    if as.length <= 0 then true
    else f(1, as(0))

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)
    
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))