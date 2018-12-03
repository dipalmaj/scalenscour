//package fpinscala.parallelism
//
//import java.util.concurrent._
//import java.util.concurrent.atomic.AtomicReference
//
//
//import language.implicitConversions
//
//object ParNonBlocking {
//  sealed trait Future[+A] {
//    private[streaming] def apply(k: A => Unit): Unit
//  }
//  type Par[+A] = ExecutorService => Future[A]
//
//  object Par {
//    //  type Par[A] = ExecutorService => Future[A]
//
//    // run could be removed and apply exposed as this is blocking...
//    // This is actually useful way of thinking of object apply usage outside of constructor
//    def run[A](es: ExecutorService)(p: Par[A]): A = {
//      val ref = new AtomicReference[A]
//
//      val latch = new CountDownLatch(1)
//
//      p(es) { a => ref.set(a); latch.countDown() }
//      // TODO Exercice 7.10
//      latch.await() // Error handling is not implemented.  Exception is swallowed.
//      ref.get
//    }
//
//    // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.
//    //  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)
//    def unit[A](a: A): Par[A] = es => new Future[A] {
//      def apply(cb: A => Unit): Unit = cb(a)
//    }
//
//    def delay[A](a: => A): Par[A] = es => new Future[A] {
//      def apply(cb: A => Unit): Unit = cb(a)
//    }
//
//    /*
//  Original
//   `map2` doesn't evaluate the call to `f` in a separate logical thread,
//   in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism.
//   We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
//
//  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
//    (es: ExecutorService) => {
//      // Could timeout be handled by tracking the time on first get call and supplying timeout to the get on future? Need to time the first one as well as supply max
//      // Wouldn't that be timeout on the entire process though? How would you track across map on single thread? Need fold?
//      val af = a(es)
//      val bf = b(es)
//      UnitFuture(f(af, bf))
//    }
//
//  */
//    def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] = es => new Future[C] {
//      def apply(cb: C => Unit): Unit = {
//        var ar: Option[A] = None
//        var br: Option[B] = None
//
//        val combiner = Actor[Either[A, B]](es) {
//          case Left(a) => br match {
//            case None => ar = Some(a)
//            case Some(b) => eval(es)(cb(f(a, b)))
//          }
//          case Right(b) => ar match {
//            case None => br = Some(b)
//            case Some(a) => eval(es)(cb(f(a, b)))
//          }
//        }
//
//        p(es)(a => combiner ! Left(a))
//        p2(es)(b => combiner ! Right(b))
//      }
//    }
//
//    /*
//    Original fork
//     This is the simplest and most natural implementation of `fork`,
//     but there are some problems with it--for one,
//     the outer `Callable` will block waiting for the "inner" task to complete.
//     Since this blocking occupies a thread in our thread pool, or
//     whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism.
//     Essentially, we're using two threads when one should suffice.
//     This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
//
//     def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {def call: A = a(es)})
//
//      Deadlock Example:
//      val a = lazyUnit(42 + 1)
//      val S = Executors.newFixedThreadPool(1) println(Par.equal(S)(a, fork(a)))
//   */
//    def fork[A](a: => Par[A]): Par[A] = es => new Future[A] {
//      def apply(cb: A => Unit): Unit = eval(es)(a(es)(cb))
//    }
//
//    /**
//      * Helper function for constructing `Par` values out of calls to non-blocking continuation-passing-style APIs.
//      * This will come in handy in Chapter 13.
//      *
//      * Previous def asyncF[A,B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))
//      */
//    def async[A](f: (A => Unit) => Unit): Par[A] = es => new Future[A] {
//      def apply(k: A => Unit): Unit = f(k)
//    }
//
//    def eval(es: ExecutorService)(r: => Unit): Unit = es.submit(new Callable[Unit] { def call: Unit = r })
//
//    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
//
//    // structure-preserving in that it doesn't alter the structure of the parallel computation, only the value "inside" the computation
//    // Previous map2(pa, unit(()))((a,_) => f(a))
//    def map[A, B](pa: Par[A])(f: A => B): Par[B] = es => new Future[B] {
//      def apply(cb: B => Unit): Unit = pa(es)(a => eval(es) {cb(f(a))})
//    }
//
//    def sequenceRight[A](as: List[Par[A]]): Par[List[A]] = as match {
//      case Nil => unit(Nil)
//      case h :: t => map2(h, fork(sequence(t)))(_ :: _)
//    }
//
//    // TODO FUN Recursion here is clunky... Is there a way to make this a better recursive function?
//    def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
//      if (as.isEmpty) unit(Vector())
//      else if (as.length == 1) map(as.head)(a => Vector(a))
//      else {
//        val (l,r) = as.splitAt(as.length/2)
//        map2(sequenceBalanced(l),sequenceBalanced(r))(_ ++ _)
//      }
//    }
//
//    def sequence[A](ps: List[Par[A]]): Par[List[A]] = map(sequenceBalanced(ps.toIndexedSeq))(_.toList)
//
//    // TODO this is funky ... is it required with this version? Get main below to work
//    def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
//      val fbs: List[Par[B]] = ps.map(unit().map(f))
//      sequence(fbs)
//    }
//
//    /*
//     * We can implement `choice` as a new primitive.
//     *
//     * `p(es)(result => ...)` for some `ExecutorService`, `es`, and
//     * some `Par`, `p`, is the idiom for running `p`, and registering
//     * a callback to be invoked when its result is available. The
//     * result will be bound to `result` in the function passed to
//     * `p(es)`.
//     *
//     * If you find this code difficult to follow, you may want to
//     * write down the type of each subexpression and follow the types
//     * through the implementation. What is the type of `p(es)`? What
//     * about `t(es)`? What about `t(es)(cb)`?
//     */
//    def choicePrimitive[A](p: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
//      es => new Future[A] {
//        def apply(cb: A => Unit): Unit =
//          p(es) { b =>
//            if (b) eval(es) { t(es)(cb) }
//            else eval(es) { f(es)(cb) }
//          }
//      }
//
//    // TODO this is fucked no? How can you check equality inside of this setup?
//    def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = p(e) == p2(e)
//
//    // TODO 7.11
//    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = ???
//
//    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = ???
//
//    // TODO 7.12
//    def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = ???
//
//    // TODO 7.13
//    def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = ???
//
//    // TODO 7.14
//    def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = ???
//
//    def join[A](a: Par[Par[A]]): Par[A] = ???
//
//    def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] = ???
//
//    def flatMapViaJoin[A,B](p: Par[A])(f: A => Par[B]): Par[B] = ???
//
//    /* Gives us infix syntax for `Par`. */
//    implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)
//
//    class ParOps[A](p: Par[A]) {
//      def map[B](f: A => B): Par[B] = Par.map(p)(f)
//      def map2[B,C](b: Par[B])(f: (A,B) => C): Par[C] = Par.map2(p,b)(f)
//      def zip[B](b: Par[B]): Par[(A,B)] = p.map2(b)((_,_))
//    }
//
//  }
//}
//
//
//object Examples {
//  import ParNonBlocking.Par._
//  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
//    if (ints.size <= 1)
//      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
//    else {
//      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
//      sum(l) + sum(r) // Recursively sum both halves and add the results together.
//    }
//
//  def main(args: Array[String]): Unit = {
//    val p = parMap(List.range(1, 10000))(math.sqrt(_))
//    val x = run(Executors.newFixedThreadPool(2))(p)
//    x.take(10).foreach(println)
//  }
//
//
//}
//
//
//
//object T {
//  def setTo = 1
//
//
//
//}
