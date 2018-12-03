package exploration.games

import exploration.games.Board.Grid

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Random, Try}

object Util {
  // Assumes a square
  // Assumes X and Y are positive
  def cordForSqrToIndex(len: Int)(y: Int, x: Int): Int = (x - 1) + len * (y - 1)

  def adjacents(len: Int)(y: Int, x: Int): Seq[(Int, Int)] = {
    val xs = Seq(x - 1, x, x + 1).filter(i => i > 0 && i <= len)
    val ys = Seq(y - 1, y, y + 1).filter(i => i > 0 && i <= len)

    ys.flatMap(yv => xs.map(xv => (yv, xv)))
  }

  def getIndexesForCordNAdj(len: Int)(y: Int, x: Int): Seq[Int] = {
    adjacents(len)(y, x).map(yNx => cordForSqrToIndex(len)(yNx._1, yNx._2))
  }

  def indexToCordOfSqr(len: Int)(index: Int): (Int, Int) = {
    val yHeight = (index + 1) / len
    val x = index % len + 1
    if((index + 1) % len > 0) (yHeight + 1, x)
    else (yHeight, x)
  }
}

// TODO adapt board to work off minefield and do prints from that rather than grid
case class Board(playerGrid: Grid, size: Int, alive: Boolean) {

  private lazy val difficulty: Int = size * size / 5

  private lazy val field = IndexedSeq.fill(size * size)(0)

  // TODO math to mimic square vector with a single array
  private lazy val minefield: IndexedSeq[Int] = {
    val mines = List.fill(difficulty*2)(Random.nextInt(size * size - 1)).toSet.take(difficulty)
    val landField = mines.foldLeft(field)((fielded, index) => fielded.updated(index, -1))
    val adjacentIndexes = mines.map(Util.indexToCordOfSqr(size)).flatMap(coords => Util.getIndexesForCordNAdj(size)(coords._1,coords._2))
    adjacentIndexes.foldLeft(landField)( (fielded, index) => {
      fielded(index) match {
        case v if v < 0 => fielded
        case v => fielded.updated(index, v + 1)
      }
    })
  }

  private def formatRow(colNum: Int, row: Seq[Int]): String = s"$colNum | " + row.mkString(" ") + " |\n"

  private def formatBorder: String = "  " + List.fill(size + 2)("-").mkString(" ")

  @tailrec
  private def printField(bGrid: Grid = playerGrid, colNum: Int = size, field: String = ""): String = bGrid match {
    case row :: rows if field.isEmpty => printField(rows, row.size - 1, formatBorder + s"\n" + formatRow(size, row))
    case row :: rows => printField(rows, colNum - 1, field + formatRow(colNum, row))
    case Nil => field + formatBorder + "\n    " + (1 to size).mkString(" ")
  }

  private def flip(index: Int, row: List[Int]): List[Int] = {
    val (front, end) = row.splitAt(index)
    (front :+ 9) ++ end.tail
  }

  private def exploded(): Board = this.copy(alive = false)

  def play(y: Int, x: Int): Board = {
    println(minefield)

    @tailrec
    def helper(oldGrid: Grid, yLvl: Int = size, out: Grid = Nil): Grid = oldGrid match {
      case row :: rows if yLvl == y => helper(rows, yLvl - 1, out :+ flip(x - 1, row))
      case row :: rows => helper(rows, yLvl - 1, out :+ row)
      case Nil => out
    }

    Board(helper(playerGrid), size, alive)
  }

  override def toString: String = printField()

}

object Board {
  type Grid = List[List[Int]]

  def apply(size: Int): Board = Board(List.fill(size)(List.fill(size)(0)), size, alive = true)
}

object SimpleGames {

  @tailrec
  def getMove(size: Int): (Int, Int) = Try(scala.io.StdIn.readLine("\nEnter coordinates Y,X \n\n").split(",").toList.map(_.toInt)).toOption match {
    case Some(y :: x :: Nil) if x > 0 && x <= size && y > 0 && y <= size => (y, x)
    case _ =>
      println(s"Invalid input. Please enter comma separated coordinates on the grid. Between 1 and $size")
      getMove(size)
  }

  @tailrec
  def run(board: Board): Unit = {
    val (y, x) = getMove(board.size)
    val updatedBoard = board.play(y, x)
    println(updatedBoard)

    if (updatedBoard.alive) run(updatedBoard)
    else {
      println("Oh no, you exploded.\nGame Over\n")
    }
  }

  def main(args: Array[String]): Unit = {
    println("TickyTacky")
    val board = Board(6)
    println(board)
    run(board)
  }

}
