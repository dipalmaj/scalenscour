package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // 3.25
  def size[A](tree: Tree[A]): Int = tree match {
    case Branch(l, r) => size(l) + size(r) + 1
    case Leaf(_) => 1
  }

  // 3.26
  def max(tree: Tree[Int]): Int = tree match {
    case Branch(l,r) => max(l) max max(r)
    case Leaf(v) => v
  }

  // 3.27
  def depth[A](tree: Tree[A], d: Int=0): Int = tree match {
    case Branch(l,r) => depth(l, d + 1) max depth(r, d + 1)
    case Leaf(_) => d
  }

  // 3.28
  def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
    case Leaf(a) => Leaf(f(a))
  }


}

object TreeBurt {
  import Tree._
  def main(args: Array[String]): Unit = {
    val t = Branch(Branch(Leaf(1), Leaf(3)), Branch(Branch(Leaf(5),Leaf(8)), Leaf(9)))

    println(size(t))
    println(max(t))
    println(depth(t))
  }
}