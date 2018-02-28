package exploration.problems

//import util.TimeSupport.stopWatch

import scala.io.Source
import scala.util.Random

case class Fences(max: Int, heightsNCnts: Vector[(Int,Int)]) {

  def add(f: Int): Fences = {
    val (fence, trimmed) = heightsNCnts.partition(_._1 < f)
    val indexAdj = { if (trimmed.isEmpty) 1 else 0 }
    // Formula is number trimmed + smallest trimmed's count + 1 for this f
    val fCnt: Int = trimmed.headOption.map(_._2 + 1).getOrElse(0)

    // Add the new addition to the fence group if it doesn't exist with counter 1
    val (retainedMax, retained) = fence.foldLeft((max,Vector[(Int,Int)]()))((acc,b) => {
      val nwCnt = b._2 + 1
      val bMax = nwCnt*b._1
      val newMax = if (bMax > acc._1) bMax else acc._1
      val newVect: Vector[(Int,Int)] = acc._2 ++ Vector((b._1, nwCnt))

      (newMax, newVect)
    })

    val fMax = f * (fCnt + indexAdj)
    val sMax = if (fMax > retainedMax) fMax else retainedMax

    val trimmedMax: Int = trimmed.foldLeft(max)((acc, t) => {
      val tArea = t._1 * t._2
      if(tArea > acc) tArea else acc
    })

    val nxtMax = if (trimmedMax > sMax) trimmedMax else sMax

    Fences(nxtMax, retained :+ (f, fCnt + indexAdj))
  }
}

object RunFences {

  def sample: Fences = {
    val in = Vector(2,5,7,4,1,8)
    val f = Fences(6, Vector((in.head,1)))
    in.tail.foldLeft(f)((acc,v) => acc.add(v))
  }

  lazy val testSize = 100000
  lazy val inputVector: Vector[Int] = Random.shuffle(1 to testSize).toVector

  val vectorTest7: Vector[Int] = {
    val lines = Source.fromFile("/Users/wanderer/Repositories/tmp/input07.txt").getLines()
    lines.next().split(",").map(_.toInt).toVector
  }


  def main(args: Array[String]): Unit = {

    val (size, input) = if (args.isEmpty){
      val lines = Source.stdin.getLines()
      (lines.next().toInt, lines.next().split(" ").map(_.toInt).toVector)
    } else {
      println(s"Running Test for Vector Size: ${vectorTest7.size}")
      (vectorTest7.size, vectorTest7)
    }

    val f = Fences(size, Vector((input.head,1)))
    val result = input.tail.foldLeft(f)((acc,v) => acc.add(v))
    println(s"Calculate Max: ${result.max}")


  }
}
