package fpinscala.errorhandling

import scala.language.implicitConversions

case class Person(name: Name, age: Age)

object Person {
  def mkName(name: String): Either[String, Name] = {
    if (name == "" || name == null) Left("Name is empty")
    else Right(new Name(name))
  }

  def mkAge(age: Int): Either[String, Age] = {
    if (age < 0) Left("Age is out of Range")
    else Right(new Age(age))
  }

  def apply(name: String, age: Int): Either[String, Person] = {
    mkName(name).map2(mkAge(age))(Person(_,_))
  }

  implicit class possiblyAPerson(pp: Either[String, Person]) {
    def print: String = pp match {
      case Right(p) => s"Person(${p.name.value},${p.age.value})"
      case Left(l) => l
    }
  }

}

sealed class Name(val value: String)
sealed class Age(val value: Int)

object Personable {

  import Person.possiblyAPerson

  def main(args: Array[String]): Unit = {
    val r = Person("joe", 12)
    println(r) // TODO try to have print use the implicit
    println(r.print)
    println(Person("joe", -1))
  }
}
