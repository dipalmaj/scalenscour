package fpinscala.laziness
sealed trait Stream[+A] {
  import Stream._

  def toList: List[A] = this match {
    case Cons(h,t) => List(h()) ++ t().toList
    case _ => Nil
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n-1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def existsFull(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().existsFull(p)
    case _ => false
  }

  // Can this be done tail rec??? Would that force evaluation?
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean = foldRight(false)((a,b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a,b) => p(a) && b)

  def headOption: Option[A] = foldRight(None: Option[A])( (a,_) => if (a != empty) Option(a) else None)

  def takeWhileFold(p: A => Boolean): Stream[A] = foldRight(Stream[A]())((a,b) => if (p(a)) cons(a, b) else empty)

  def filter(p: A => Boolean): Stream[A] = foldRight(Stream[A]())((a,b) => if (p(a)) cons(a,b) else b)

  def map[B](p: A => B): Stream[B] = foldRight(Stream[B]())((a,b) => cons(p(a), b))

  def flatMap[B](p: A => Stream[B]): Stream[B] = foldRight(Stream[B]())((a,b) => p(a) match {
    case Cons(h, _) => cons(h(),b)
    case _ => empty
  })

  // TODO asInstance required?
  def append[C >: A](n: C): Stream[A] = foldRight(Stream[A]())((a,b) => b match {
    case Empty => cons(a, cons(n.asInstanceOf[A], Empty))
    case _ => cons(a, b)
  })

  // General Stream Building
  def unfold[A,S](z:S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h, e)) => cons(h, unfold(e)(f))
    case None => empty
  }

  // n, n +1 ...
  def from(n: Int): Stream[Int] = unfold(n)(up => Some(up,up+1))

  // Fib 0,1,1,2,3,5,8,13,21,34, ...
  def fib: Stream[Int] = unfold((0,0))(up => {
    if (up._1 == 0 && up._2 == 0) Some(0, (1,0))
    else if (up._1 == 1 && up._2 == 0) Some(1, (0,1))
    else if (up._1 == 0) Some(up._1 + up._2, (1, 1))
    else Some(up._1 + up._2, (up._2, up._1 + up._2))
  })

  // Use unfold to map2
  def mapU[B](p: A => B): Stream[B] = unfold(this)({
    case Cons(h,t) => Some((p(h()),t()))
    case _ => None
  })

  def zipWith[B](s2: Stream[B]): Stream[(A,B)] = unfold((this, s2))( sm => (sm._1, sm._2) match {
    case (Cons(h1,t1), Cons(h2,t2)) => Some(((h1(), h2()), (t1(), t2())))
    case _ => None
  })

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = unfold((this, s2))( sm => (sm._1, sm._2) match {
    case (Cons(h1,t1), Cons(h2,t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
    case (Cons(h1,t1), Empty) => Some(((Some(h1()), None), (t1(), Empty)))
    case (Empty, Cons(h2,t2)) => Some(((None, Some(h2())), (Empty, t2())))
    case _ => None
  })

  // Additional
  def startsWith[A](s2: Stream[A]): Boolean = this.zipWith(s2).forAll(streams => streams._1 == streams._2)

  // Could use option rather than tuple but match won't unpack two levels I don't think...
  def tails: Stream[Stream[A]] = unfold((this,true))({
    case (Cons(h, t),_) => Some((cons(h(),t()), (t(),true)))
    case (Empty,true) => Some((empty, (empty,false)))
    case (Empty,false) => None
  })

  def hasSubsequence[A](s: Stream[A]): Boolean = tails.exists(_.startsWith(s))

  // TODO fix me
  def scanRight[C >: A](z: C)(f: (C, => C) => C): Stream[C] = unfold(this.tails)({
    case Cons(h : Stream[C],t) => Some((h.foldRight(z)(f), t()))
    case _ => None
  })

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  // TODO is there a way to update this so it doesn't evaluate parameters
  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}


object Streaming {
  def main(args: Array[String]): Unit = {
    // Unfolding
    val pp = Stream[Int]()
    pp.fib.take(15).toList
    pp.from(2).take(5).toList

    // More unfolding
    val res = pp.unfold(10)(pop => if(pop < 10000) Some(pop,pop+1) else None)
    res.take(4).toList
    pp.from(20).take(10).toList


    val s = Stream(1,2,3,0)
    s.tails.toList.map(_.toList)
    s.foldRight(0)(_ + _)

    println(s)
    s.take(1).toList
    s.takeWhile(_ < 3).toList
    s.forAll(_ < 10)
    s.headOption
    s.filter(_ < 2).toList
    s.append(4).toList
    s.map(_ * 10).toList
    s.flatMap(v => Stream(v.toString)).toList
    Stream.cons({println(1); 1},Stream.cons({println(2); 2},Stream.cons({println(3); 3},Stream.cons({println(4); 4}, Stream.empty)))).takeWhileFold(_ < 3).toList
    s.mapU(_ * 5).toList
    Stream.cons({println(1); 1},Stream.cons({println(2); 2},Stream.cons({println(3); 3},Stream.cons({println(4); 4}, Stream.empty)))).takeWhileFold(_ < 3).take(0).toList

    val ss = Stream(1,2)
    s.startsWith(ss)

    val b = Stream(7,8,9)
    s.zipWith(b).toList
    s.zipAll(b).toList
    s.startsWith(b)


    val x = List(1,2,3,4)
    val y = List("a","b","c")
    x.zipAll(y,5,"z")

  }
}