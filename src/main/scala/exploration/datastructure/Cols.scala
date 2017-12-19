package exploration.datastructure

import scala.annotation.tailrec

/**
  * Created by wanderer on 4/25/17.
  */
sealed trait Cols

sealed trait AForm[+A] {
  def head: A
  def tail: AForm[A]
}

case object AFNil extends AForm[Nothing] {
  override def head: Nothing = throw new Throwable("ere")

  override def tail: AForm[Nothing] = AForm[Nothing]()
}

case class AFCons[+A](head: A, tail: AForm[A]) extends AForm[A]

object AForm {

  def foldRight[A,B](af: AForm[A], z: B)(f: (A,B) => B): B = {
    foldLeft(reverse(af), z)((d: B, e: A) => f(e, d))
  }

  @tailrec
  def foldLeft[A,B](af: AForm[A], z: B)(f: (B,A) => B): B = af match {
    case AFCons(a, afs) => foldLeft(afs,f(z,a))(f)
    case AFNil => z
  }

  def bump(af: AForm[Int]): AForm[Int] = {
    foldRight(af,AForm[Int]())((y,x) => AFCons(y + 1, x))
  }

  def setHead[A](af: AForm[A], a: A) = AFCons(a, af.tail)

  def lengthR[A](af: AForm[A]): Int = {
    foldRight(af, 0)((x,y) => y + 1)
  }

  def length[A](af: AForm[A]): Int = {
    foldLeft(af,0)((y,x) => y + 1)
  }

  def reverse[A](af: AForm[A]): AForm[A] = {
    foldLeft(af,AForm[A]())((y,x) => AFCons(x, y))
  }

  def drop[A](af: AForm[A], n: Int): AForm[A] = af match {
    case AFCons(_, x) if n != 0 => drop[A](x, n - 1)
    case _ => af
  }

  def dropWhile[A](af: AForm[A])( f: A => Boolean): AForm[A] = af match {
    case AFCons(a, afs) if f(a) => dropWhile(afs)(f)
    case _ => af
  }

  def append[A](af: AForm[A], a: A): AForm[A] = {
    foldLeft(af, AForm(a))((x,y) => AFCons(y,x))
  }

  def ++[A](af: AForm[A], naf: AForm[A]): AForm[A] = {
    foldLeft(reverse(naf), af)((x,y) => AFCons(y,x))
  }

  // Why are these reverse required? Should be possible to do without no?
  def map[A,B](af: AForm[A])(f: A => B): AForm[B] = {
    foldLeft(reverse(af), AForm[B]())((x,y) => AFCons(f(y), x))
  }

  def flatMap[A,B](af: AForm[A])(f: A => AForm[B]): AForm[B] = {
    foldLeft(reverse(af), AForm[B]())((x,y) => ++(x,f(y)))
  }

  def filter[A](af: AForm[A])(f: A => Boolean): AForm[A] = {
    flatMap(reverse(af))(x => if (f(x)) AForm(x) else AFNil)
//    foldLeft(reverse(af), AForm[A]())((x,y) => if (f(y)) AFCons(y, x) else x)
  }

  // Make appropriately generic
  def combine(af: AForm[Int], bf: AForm[Int]): AForm[Int] = {
    ???
  }

  def zipWith[A](seqA: AForm[A], seqB: AForm[A]): AForm[A] = ???

  def apply[A](es: A*): AForm[A] = {
    if (es.isEmpty) AFNil
    else AFCons[A](es.head, apply(es.tail: _*))
  }
}