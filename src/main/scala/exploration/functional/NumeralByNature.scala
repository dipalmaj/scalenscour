package exploration.functional


trait NumeralByNature[A] {
  def plus(a: A, b: A): A
  def minus(a: A, b: A): A
  def divide(a: A, b: A): A
  def multiply(a: A, b: A): A
  def empty: A
}


object NumeralByNatureUtil {

  implicit object IntNumeral extends NumeralByNature[Int] {

    override def plus(a: Int, b: Int): Int = a + b

    override def minus(a: Int, b: Int): Int = a - b

    override def divide(a: Int, b: Int): Int = a / b

    override def multiply(a: Int, b: Int): Int = a * b

    override def empty: Int = 0
  }

  implicit object DblNumeral extends NumeralByNature[Double] {

    override def plus(a: Double, b: Double): Double = a + b

    override def minus(a: Double, b: Double): Double = a - b

    override def divide(a: Double, b: Double): Double = a / b

    override def multiply(a: Double, b: Double): Double = a * b

    override def empty: Double = 0.0
  }

  implicit class NumeralUtil[T](x: T)(implicit number: NumeralByNature[T]) {

    def add(that: T): T = number.plus(x, that)

    def product(that: T): T = number.multiply(x, that)

    def divide(that: T): T = number.divide(x, that)

    def multiply(that: T): T = number.multiply(x, that)

  }

  def empty[T](implicit number: NumeralByNature[T]): T = number.empty

}
