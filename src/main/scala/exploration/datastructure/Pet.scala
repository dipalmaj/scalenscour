package exploration.datastructure

import java.sql.Timestamp

import scala.annotation.tailrec
import scala.reflect.ClassTag

sealed trait Pet {
  type T

  def value: T
}

case class Dog(value: String) extends Pet {
  type T = String
}

case class Cat(value: String) extends Pet {
  type T = String
}
case class Start(value: Timestamp) extends Pet {
  type T = Timestamp
}

case class End(value: Timestamp) extends Pet {
  type T = Timestamp
}


// Goal Better way to do
// ua.payload[Start].value.toDateTime.between(trialEnd.minusHours(EARLY_PURCHASE_HOURS), graceEnd.plusHours(LATE_PURCHASE_HOURS))
object Test {

  def get[P <: Pet : ClassTag](pets: List[Pet]): Option[P#T] = {
   @tailrec
    def helper(remainingPets: List[Pet]): Option[P#T] = remainingPets match {
     case (x: P) :: _ => Option(x.value)
     case _ :: xs => helper(xs)
     case Nil => None
   }

    helper(pets)
  }


  def main(args: Array[String]): Unit = {
    val pets: List[Pet] = List(Dog("pippy"), Cat("tabby"))

    val retrieved = get[Dog](pets)

    println(retrieved)

  }
}