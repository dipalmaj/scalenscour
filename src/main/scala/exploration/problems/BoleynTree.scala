package exploration.problems

import scala.annotation.tailrec
import scala.collection.immutable.{HashMap, SortedMap}
import scala.io.Source


object BoleynTree {

  val splitter: String => Seq[Int] = _.split(" ").map(_.toInt)

  def main(args: Array[String]): Unit = {
    // Standard In read
    // val input = Source.stdin.getLines().toIndexedSeq
    val input = Source.fromFile("/Users/wanderer/Downloads/boleyn-salary-testcases/input/input01.txt").getLines()

    val numberOfEmployees = splitter(input.next).head

    val employeeTree = input.take(numberOfEmployees - 1).foldLeft(HashMap(1 -> List[Int]()))( (acc, b) => {
      val employeeIdAndSuperior = splitter(b)
      acc + (employeeIdAndSuperior.last -> (employeeIdAndSuperior.head :: acc(employeeIdAndSuperior.last)), employeeIdAndSuperior.head -> Nil)
    })

    val salary = splitter(input.next).toArray

    val trimmer = (kLowest: Int, emps: SortedMap[Int,Int]) => if (emps.size > kLowest) emps.dropRight(1) else emps

    @tailrec
    def makeSalMap(r: List[Int], kLowest:Int, acc: SortedMap[Int, Int]): SortedMap[Int,Int] = r match {
      case id :: ids => makeSalMap(employeeTree(id) ::: ids, kLowest, trimmer(kLowest, acc + (salary(id-1) -> id)))
      case Nil => acc
    }

    input.foldLeft(0)((prevAnswer, nxtQuery) => {
        val vAndk = splitter(nxtQuery)
        val id = vAndk.head + prevAnswer
        val result = makeSalMap(employeeTree(id),vAndk.last, SortedMap[Int,Int]()).last._2
        println(result)
        result
      })

  }

}

