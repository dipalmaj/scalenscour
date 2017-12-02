package exploration.implicitly

case class Employee(name: String)

trait Comparator[T] {
  def compare(o1: T, o2: T): Int
}

object Collections1 {
  // Inefficient quick sort implementation
  def sort[T](xs: Seq[T], comparator: Comparator[T]): Seq[T] = xs match {
    case Seq() => Seq.empty
    case head +: tail =>
      val (st, gte) = tail.partition(comparator.compare(xs.head, _) > 0)
      sort(st, comparator) ++ Seq(head) ++ sort(gte, comparator)
  }
}

object Collections {
  def sort[T](xs: Seq[T])(implicit comparator: Comparator[T]): Seq[T] = xs match {
    case Seq() => Seq.empty
    case head +: tail =>
      val (st, gte) = tail.partition(comparator.compare(xs.head, _) > 0)
      sort(st) ++ Seq(head) ++ sort(gte)
  }
}

object CompareMe {

  implicit object EmployeeComparator extends Comparator[Employee] {
    override def compare(o1: Employee, o2: Employee): Int =
      o1.name.compareTo(o2.name)
  }

  implicit def optionComparator[T](implicit innerComparator: Comparator[T]) = new Comparator[Option[T]] {
    override def compare(o1: Option[T], o2: Option[T]): Int =
      (o1, o2) match {
        case (None, None) => 0
        case (None, Some(_)) => -1
        case (Some(_), None) => 1
        case (Some(e1), Some(e2)) => innerComparator.compare(e1, e2)
      }
  }

}



object Test {
  def main(args: Array[String]): Unit = {

    val x: (Int, Int, Int => Int) => Seq[Int] = (s: Int, e: Int, f: Int => Int) => (s to e).map(f)

  }


}