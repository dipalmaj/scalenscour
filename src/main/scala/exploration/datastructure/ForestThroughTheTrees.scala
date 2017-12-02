package exploration.datastructure

/**
  * Don't blame me, it sounded good at the time
  */
object ForestThroughTheTrees {

  def runAndHide(): Unit = {

  }

}

sealed trait Origin {
  type T
  def key: Int
  def value: T

}

case class Teacher(x: Int) {
  def and(that: Teacher): Teacher = if (x > 2) that else this
  def or(that: Teacher): Teacher = if (x < 2) this else that
  def complement: Teacher = Teacher(x)
//  def not(x: Teacher) = x.complement
//  def xor(x: Teacher, y: Teacher) = (x or y) and not(x and y)
}

case class Malcolm[T](key: Int, value: T, middle: Boolean, siblings: Int)// extends Origin

case class LittleDevil[T](key: Int, value: T, youngest: Boolean, gap: Int)// extends Origin

case class PerfectAngel[T](key: Int, value: T, oldest: Boolean, gap: Int)// extends Origin

case class Temp(x: Boolean) {
  def and(that: Temp): Temp = if (x) that else this
  def or(that: Temp): Temp = if (x) this else that
  def complement: Temp = Temp(!x)
  def not(x: Temp) = x.complement
  def xor(x: Temp, y: Temp) = (x or y) and not(x and y)
}
