package exploration.problems

import util.TimeSupport.stopWatch

case class Die(top: Int, front: Int, right: Int)
object DiceBoard {
  type DieValue = (Int, Die)
  type BoardValue = (Int, Set[(Int,Die)])
  type Board = collection.immutable.Map[(Int,Int), BoardValue]

  val down: (DieValue => DieValue) = (vd: (Int, Die)) => (vd._1 + (7-vd._2.front), Die(7 - vd._2.front, vd._2.top, vd._2.right))
  val right: (DieValue => DieValue) = (vd: (Int,Die)) => (vd._1 + (7-vd._2.right), Die(7 - vd._2.right, vd._2.front, vd._2.top))

  def move(node: BoardValue, f: DieValue => DieValue): BoardValue = {
    val moved = node._2.map(f)
    (moved.maxBy(_._1)._1, moved)
  }

  def mergeBoardValues(bv1: BoardValue, bv2: BoardValue): BoardValue = {
    (Seq(bv1._1, bv2._1).max, bv1._2 ++ bv2._2)
  }

  def maxMerge(boards: Board*): Board = {
    if (boards.size == 1) boards.head
    else
      boards.tail.foldLeft(boards.head)((acc, brd) => {
      acc ++ brd ++ acc.keySet.intersect(brd.keySet).map( k => (k, mergeBoardValues(acc(k), brd(k))))
    })
  }

  def apply(board: Board, m: Int, n: Int): (Int, Board) = {
    if (board.contains(m,n)) (board(m,n)._1, board)
    else {
      val keys = board.keySet.filterNot(mn => mn._1 > m && mn._2 > n)
      val maxCoords = Set(keys.filter(_._2 == 1).maxBy(_._1),keys.filter(_._1 == 1).maxBy(_._2)).toSeq.sortBy(_._1)
      val buildSquare = if (m > n) m else n

      val fill = maxCoords.flatMap(coord => {
        (coord._1 to buildSquare).flatMap(cm => {
          val newColStart = if (cm > coord._1) 1 else coord._2
          (newColStart to buildSquare).map(cn => (cm,cn))
        })
      }).sorted

      val expanded = fill.foldLeft(board)((acc, mn) => {
        if (acc.contains(mn)) acc
        else {
          expand(acc, mn)
        }
      })
      (expanded(m,n)._1, expanded)
    }
  }

  // Start condition (1,1)
  def expand(board: Board, coord: (Int,Int)): Board = {
    val (m,n) = coord
    if (m == 1) {
      maxMerge(board, Map(coord -> move(board(m,n-1), right)))
    }
    else if (n == 1) {
      maxMerge(board, Map(coord -> move(board(m-1,n), down)))
    }
    else maxMerge(board, Map(coord -> mergeBoardValues(move(board(m,n-1), right), move(board(m-1,n), down))))
  }

}

object DicePath {
  import DiceBoard._

  /*
  Initially dice is at point (1, 1), and its top face has 1 pip, front face has 2 pips,
  and left face has 3 pips.

  A path sum to a point is the sum of value of dice when it is rolled to that point from (1, 1).
  As already stated, value at the current location is the number of pips on the top face of the dice.
  Find the maximum path sum to (M, N).

   */
  val die = Die(1, 2, 4)
  val initialStates = Set((1,die))
  val initialBoard: Board = Map((1,1) -> (1, initialStates))

  def main(args: Array[String]): Unit = {
    simpleTest

    val r = stopWatch(DiceBoard(initialBoard, 1, 60))
    println(r._1)
  }

  def simpleTest = {
    assert(9 == DiceBoard(initialBoard, 2,2)._1, "Incorrect Value 9")
    assert(4 == DiceBoard(initialBoard, 1,2)._1, "Incorrect Value 4")
    assert(6 == DiceBoard(initialBoard, 2,1)._1, "Incorrect Value 6")
    assert(19 == DiceBoard(initialBoard, 3,3)._1,"Incorrect Value 19")

    val test = DiceBoard(initialBoard, 2, 3)
    println(s"Results were ${test._1}")
    println(s"New board is ${test._2}")

    val test2 = DiceBoard(initialBoard, 20, 22)
    println(s"Results were ${test2._1}")
  }

}
