package dot.functional

/**
  * Functional Scala 3.5
  */
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(x) => 1
      case Branch(l,r) => size(l) + size(r)
    }
  }

  def maximum[A](tree: Tree[A]): A = ???

  def depth[A](tree: Tree[A], value: A): Int = ???

  def map[A,B](tree: Tree[A], func: A => B): Tree[B] = ???

  def fold[A,B](tree: Tree[A])(func: Tree[A] => Tree[B]) = ???

  def append[A, B <: Tree[A]](tree: Tree[A], growth: B): Tree[A] = ???
}

