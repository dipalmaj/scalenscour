package exploration.glitch

import dot.functional.{Branch, Leaf}
import dot.functional.Tree._
/**
  * Created by wanderer on 5/5/17.
  */
object Bowser {

  def main(args: Array[String]): Unit = {
    val r = scala.util.Random
    val ranInties = (1 to 30).map(i => r.nextInt(1000))
    println(ranInties)

    val t = Branch(Leaf(1),Branch(Branch(Leaf(2),Leaf(4)),Branch(Leaf(9), Leaf(29))))
    println(size(t))
  }
}
