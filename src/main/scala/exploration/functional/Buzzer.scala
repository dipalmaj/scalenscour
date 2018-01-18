package exploration.functional

case class Buzzer(entry: Option[Int]) {
  def getFizzy: Option[String] = entry.collect{
    case x if x % 5 == 0 && x % 3 == 0 => "fizzbuzz"
    case x if x % 5 == 0 => "buzz"
    case x if x % 3 == 0 => "fizz"
  }
}

object Buzzer {
  def apply(entry: Int): Buzzer = Buzzer(Option(entry))
}

object TestBuzz {

  def test(entries: Int*): String = entries.flatMap(e => Buzzer(e).getFizzy).mkString(" & ")

  assert(test(1,2,7).isEmpty)
  assert(test(10,11,5,2,7) == "buzz & buzz")
  assert(test(10,11,6,30,7) == "buzz & fizz & fizzbuzz")
  assert(test(120,10,11,5,3,7) == "fizzbuzz & buzz & buzz & fizz")

}
