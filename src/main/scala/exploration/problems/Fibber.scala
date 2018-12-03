package exploration.problems

import scala.annotation.tailrec

object Fibber {

  def fib(n: Int): Int = {
    @tailrec
    def helper(n1: Int, n2: Int, cnt: Int = 1): Int = {
      if (cnt == n) n1 + n2
      else helper(n1+n2, n1, cnt + 1)
    }

    if (n < 2) n
    else helper(0,1)
  }

  def main(args: Array[String]): Unit = {
    (0 to args.headOption.map(_.toInt).getOrElse(10)).foreach(i => println(s"Fib($i) == ${fib(i)}"))
  }

}



