package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.

  /*
  Provided Implementations
   */
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    {
      foldLeft(as, z)((d: B, e: A) => f(e,d))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  /*
   Exercises
   */

  // Exercise 3.1
  val z = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  // Exercise 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, xs) => xs
    case _ => Nil
  }

  // Exercise 3.3
  def setHead[A](l: List[A], h: A): List[A] = Cons(h, tail(l))

  // Exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] = n match {
    case i if i > 0 => drop(tail(l), n-1)
    case _ => l
  }

  // Exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match  {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  // Ex 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
    case _ => Nil
  }

  def length[A](l: List[A]): Int = foldRight(l,0)((_,y) => y + 1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Cons(x, xs) => foldLeft(xs, f(z,x))(f)
    case _ => z
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Cons(x, xs) => foldLeft(xs, Cons(f(x), Nil))((z,y) => Cons(f(y),z))
    case _ => Nil
  }
}


object Test {
  import List._

  def main(args: Array[String]): Unit = {
    val test = Cons(3, Cons(2, Cons(1, Nil)))
    println(init(test))
    println(init(init(test)))

    println(map(test)(_ * 2))

    foldLeft(test, 0)( (b,a) => {
      val c = b+a
      println(c)
      c
    })

    println("Right")
    foldRight(test, 0)( (a,b) => {
      val c = b+a
      println(c)
      c
    })
  }
}