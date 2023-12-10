object ChapterThree {

  enum MyList[+A]:
    // constructors
    case Nil
    case Cons(head: A, tail: MyList[A])

  object MyList:
    // companion object
    def apply[A](as: A*): MyList[A] =
    // A * -> variadic function syntax
      if as.isEmpty then Nil
      else Cons(as.head, apply(as.tail *))

    def count[A](as: MyList[A]): Int = as match
      case Nil => 0
      case Cons(x, xs) => 1 + count(xs)

    def sum(ints: MyList[Int]): Int = ints match
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)

    val exercise1 = MyList(1, 2, 3, 4, 5) match
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y

    // exercise2
    def tail[A](as: MyList[A]): MyList[A] = as match
      case Nil => Nil
      case Cons(x, xs) => xs

    // exercise3
    def setHead[A](as: MyList[A], y: A): MyList[A] = as match
      case Nil => Nil
      case Cons(x, xs) => Cons(y, xs)

    // exercise 4
    def dropN[A](as: MyList[A], n: Int): MyList[A] =
      if n > 0
      then
        as match
          case Nil => Nil
          case Cons(x, xs) => dropN(xs, n - 1)
      else as

    // exercise 5
    def dropWhile[A](as: MyList[A], f: A => Boolean): MyList[A] =
      as match
        case Nil => Nil
        case Cons(x, xs) =>
          if f(x)
          then dropWhile(xs, f)
          else Cons(x, dropWhile(xs, f))

  // exercise 6
    def init[A](as: MyList[A]): MyList[A] =
      as match
        case Nil => Nil
        case Cons(x, xs) =>
          if xs == Nil
          then Nil
          else Cons(x, init(xs))

  val ex1: MyList[Double] = MyList.Nil
  val ex2: MyList[Int] = MyList.Cons(1, MyList.Nil)
}
