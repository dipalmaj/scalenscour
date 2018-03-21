package exploration.problems

import scala.collection.immutable.HashMap


case class BoleynTree(salary: Int, reports: Set[Int]=Set())

object BoleynTree {

  val input = """8 7
2 1
3 2
4 2
7 4
8 4
5 1
6 5
70 40 60 80 10 20 30 50
2 1
-6 5
-4 1
-5 3
2 1
-5 4
2 2
"""

  val i = scala.io.Source.stdin.getLines()
  //i.next()
  //i.toIndexedSeq

  // TODO use vector index lookup vs list with head tails? or array
  val d = input.split("\n")
  val empQueries = d.head.split(" ").map(_.toInt)
  val emplys = d.tail.take(empQueries.head-1).map(_.split(" ").toVector.map(_.toInt))
  val sals = d(empQueries.head).split(" ").map(_.toInt).toVector
  val querys = d.drop(empQueries(0) + 1)



  def runQueries(employees: IndexedSeq[Vector[Int]], salaries: Vector[Int], queries: IndexedSeq[String]) = {
    val tree = employees.
      foldLeft(HashMap(1 -> BoleynTree(salaries.head)))((acc, uAndp) => {
        val boss = acc(uAndp.last)
        acc + (uAndp.last -> boss.copy(reports = boss.reports + uAndp.head) , uAndp.head -> BoleynTree(salaries(uAndp.head-1)))
      })

    // Could optimize and put this inside so tree is available
    // Also the accumulator here needs to preserve order so maybe an optimized structure for it or system to preserve
    def getSubordinates(id: Int, subStack: Set[Int]=Set(), acc: Seq[(Int,Int)]=Nil): Seq[(Int,Int)] = {
      val as  = tree(id)
      if (as.reports.isEmpty && subStack.isEmpty) acc :+ (id, as.salary)
      else if (as.reports.nonEmpty) getSubordinates(as.reports.head, as.reports.tail.union(subStack), acc :+ (id, as.salary))
      else getSubordinates(subStack.head, subStack.tail, acc :+ (id, as.salary))
    }

    queries.foldLeft(0)((prevAnswer, queryStr) => {
      val vAndk = queryStr.split(" ").map(_.toInt)
      val id = vAndk.head + prevAnswer
      val place = tree(id).reports
      val x = getSubordinates(place.head, place.tail).sortBy(_._2)
      val kMember = x(vAndk.last - 1)
      println(kMember._1)
      kMember._1
    })
  }

  runQueries(emplys, sals, querys)

}

