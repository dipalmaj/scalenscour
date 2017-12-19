package fpinscala.datastructures

import org.scalatest.FunSuite

class ListTest extends FunSuite {

  val first = Cons(2, Nil)

  test("testDrop") {
    assertResult(first)(List.drop(Cons(1,Cons(3, first)),2))
  }

  test("testProduct2") {

  }

  test("testSum") {

  }

  test("testFoldLeft") {

  }

  test("testMap") {

  }

  test("testTail") {
    assertResult(first)(List.tail(Cons(6, first)))
  }

  test("testSetHead") {
    assertResult(Cons(10, Nil))(List.setHead(first, 10))
  }

  test("testDropWhile") {

  }

  test("testSum2") {

  }

  test("testFoldRight") {

  }

}
