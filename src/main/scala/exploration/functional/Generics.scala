package dot.functional

/**
  * Created by wanderer on 4/25/17.
  */
object Generics {

  def partial[A,B,C](a: A, f: (A,B) => C): B => C = {
    (b: B) => f(a,b)
  }

  def curry[A,B,C](f: (A,B) => C): A => (B => C) = (a: A) => (b: B) => f(a,b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = ???
}
