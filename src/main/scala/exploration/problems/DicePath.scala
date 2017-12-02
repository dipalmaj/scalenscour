package exploration.problems

import scala.io.Source
import scala.util.Try

sealed trait Move {
  type DieState[S,+A] = S => (A,S)
  def move: DieState[SixSided,Int]
  def apply(ds: SixSided) = move(ds)
}
case object Down extends Move {
  override def move: DieState[SixSided,Int] = ss => {
    val front = 7 - ss.front
    (front, SixSided(front, ss.top, ss.right))
  }
}
case object Right extends Move {
  override def move: DieState[SixSided,Int] = ss => {
    val left = 7 - ss.right
    (left, SixSided(left, ss.front, ss.top))
  }
}
case class SixSided(top: Int, front: Int, right: Int) {
  def ==(die: SixSided): Boolean = {
    this.top == die.top && this.front == die.front && this.right == die.right
  }
}

object DicePath {
  // StopWatch
  def stopWatch[A](process: => A, task: String=""): A = {
    val start = System.currentTimeMillis()
    val result = Try(process)
    val stop = System.currentTimeMillis()
    val timeDiff = stop - start

    lazy val hours = (timeDiff / (1000 * 60 * 60)) % 24
    lazy val minutes = (timeDiff / (1000 * 60)) % 60
    lazy val seconds = (timeDiff / 1000) % 60
    lazy val milliseconds = timeDiff / 1000

    // format time diff in HH:MM:SS:MMM
    lazy val elapsedTime = "%02d:%02d:%02d.%03d".format(hours, minutes, seconds, milliseconds)

    println(s"Task: $task Time: $elapsedTime")
    result.get
  }

  /*
  Initially dice is at point (1, 1), and its top face has 1 pip, front face has 2 pips,
  and left face has 3 pips.

  A path sum to a point is the sum of value of dice when it is rolled to that point from (1, 1).
  As already stated, value at the current location is the number of pips on the top face of the dice.
  Find the maximum path sum to (M, N).
   */
  val die = SixSided(1, 2, 4)
  val mStart = 1
  val nStart = 1


  def runTest(test: String) = {
    val lines = Source.fromFile(s"/Users/wanderer/Documents/data/hackerrank/$test").getLines()
    lines.next()

    // Grid (M, N)
  }

  def main(args: Array[String]): Unit = {

    val repeats = pattern(20).distinct
    println(s"There are ${repeats.size} repeats")
    repeats.foreach(println)

    // Testing
//    runTest("test3")

//    println(stopWatch(getMaxPathTo(10,20)._1, "SoloRun"))
  }

  def pattern(attempts: Int, dice: SixSided=die, moves: Seq[Move]=Nil, results: Seq[Seq[Move]]=Nil): Seq[Seq[Move]] = {
    if (attempts > 0) {
      val movedRight = Right(dice)
      val movedDown = Down(dice)

      {
        if (movedRight._2 == die) {
          val mvs = moves :+ Right
          results :+ mvs
        } else pattern(attempts-1, movedRight._2, moves :+ Right, results)
      } ++
      {
        if (movedDown._2 == die) {
          val mvs = moves :+ Down
          results :+ mvs
        } else pattern(attempts-1, movedDown._2, moves :+ Down, results)
      }
    }
    else results
  }


  def test(): Unit = {

  }

}
