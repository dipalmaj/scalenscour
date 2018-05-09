package exploration.games

object Soduku {

  type Board = Map[(Int,Int), Int]
  type PBoard = Map[(Int,Int), Set[Int]]

  // Maybe use numbers?
  /// Display Board?

  private val vertical = (0 to 8).toList
  private val horizontal = vertical
  private val initial = vertical.flatMap(v => horizontal.map(h => ((v,h), 0))).toMap

  private val possible = (1 to 9).toSet


  /* Build Board Right Side?
  {
    v <- vertical
    h <- horizontal
  } yield (vh)
  */

  def pSquare(topX: Int, topY: Int, b: Board): Set[Int] = {
    val filled = (topX to topX + 2).flatMap(x => {
      (topY to topY + 2).map(y => b(x,y))
    }).filterNot(_ == 0).toSet

    possible.diff(filled)
  }

  def sqrCoords(x: Int, y: Int): Int = {
    val sx = if (x < 3) 0 else if (x < 6) 1 else 2

    val coord = if (y < 3) sx else if (y < 6) 3 + sx else 6 + sx

    println(coord)
    coord
  }

  def solve(b: Board): Board = {
    val sqrs = Array(
      pSquare(0,0, b),
      pSquare(0,3, b),
      pSquare(0,6, b),
      pSquare(3,0, b),
      pSquare(3,3, b),
      pSquare(3,6, b),
      pSquare(6,0, b),
      pSquare(6,3, b),
      pSquare(6,6, b)
    )
    val rows = vertical.map(v => {
      horizontal.foldLeft(possible)((acc, h)  => {
        val p = b(v,h)
        if (p != 0) acc - p
        else acc
      })
    }).toIndexedSeq

    val cols = horizontal.map(h => {
      vertical.foldLeft(possible)((acc, v)  => {
        val p = b(v,h)
        if (p != 0) acc - p
        else acc
      })
    }).toIndexedSeq

//    rc.foreach(println)

    println(s"Grid Possibles:\t${sqrs(0)-6}\n")
    val rowInGrid = rows.take(3).map(_.intersect(sqrs(0)-6))
    val colsInGrid = cols.take(3).map(_.intersect(sqrs(0)-6))

//    println(s"Rows in Grid:\t${rows(0)}\t${rows(1)}\t${rows(2)}")
    println(s"Crossover rows $rowInGrid\n")
//    println(s"Column in Grid:\t${rows(0)}\t${rows(1)}\t${rows(2)}")
    println(s"Crossover cols $colsInGrid")




    printBoard(b)
    b ++ Map((2,2) -> 6, (2,6) -> 8)
  }


  def main(args: Array[String]): Unit = {

    val board = initial ++ Map(
      (0,1) -> 9,
      (0,5) -> 5,
      (1,1) -> 2,
      (1,2) -> 8,
      (1,4) -> 9,
      (1,5) -> 4,
      (2,0) -> 5,
      (2,1) -> 4,
      (2,3) -> 3,
      (2,4) -> 7,
      (2,5) -> 2,
      (2,7) -> 9,
      (2,8) -> 1,
      (3,2) -> 5,
      (3,3) -> 2,
      (3,4) -> 3,
      (3,5) -> 8,
      (4,2) -> 3,
      (4,6) -> 5,
      (5,3) -> 1,
      (5,4) -> 5,
      (5,5) -> 9,
      (5,6) -> 7,
      (6,0) -> 4,
      (6,1) -> 6,
      (6,3) -> 5,
      (6,4) -> 8,
      (6,5) -> 3,
      (6,8) -> 1,
      (6,9) -> 7,
      (7,3) -> 9,
      (7,4) -> 4,
      (7,6) -> 2,
      (7,7) -> 5,
      (8,3) -> 7,
      (8,7) -> 4
    )
    printBoard(board)
    val nxt = solve(board)
    solve(nxt)
  }

  def printBoard(board: Board): Unit = {
    def formatRows(start: Int, end: Int): String = {
      (start to end).map(v => {
        val row = horizontal.map(h => board((v, h))).toIterator
        row.take(3).mkString(" ") + " | " + row.take(3).mkString(" ") + " | " + row.mkString(" ")
      }).mkString("\n")
    }

    val hBreaks = List.fill(3)("--").mkString("")
    val hz: String => String = (del: String) =>  s"$hBreaks$del$hBreaks$del$hBreaks"

    println(hz(" "))
    println(formatRows(0,2))
    println(hz("+"))
    println(formatRows(3,5))
    println(hz("+"))
    println(formatRows(6,8))
    println(hz(" "))
  }
}
