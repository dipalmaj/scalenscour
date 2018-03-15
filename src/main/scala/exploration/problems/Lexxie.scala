package exploration.problems

import util.IOHelpers
import util.TimeSupport.stopWatch

import scala.annotation.tailrec


/** Code for HackerRank Password Cracker FP
  *
  * https://www.hackerrank.com/challenges/password-cracker-fp/problem
  *
  * @param value
  * @param kids
  */


case class Node(value: Char, kids: Set[Node])

object Lexxie {
  def buildFromWord(word: String, tree: List[Node] = Nil): List[Node] = {
    val chars = word.toCharArray

    @tailrec
    def traverse(lts: Array[Char], currentNode: Node, stack: List[Node] = Nil): Node = {
      if (lts.isEmpty && stack.nonEmpty) traverse(lts, stack.head.copy(kids = stack.head.kids.filterNot(_.value == currentNode.value) + currentNode), stack.tail)
      else if (lts.nonEmpty) {
        val nxtChar = lts.head
        currentNode.kids.find(_.value == nxtChar) match {
          case Some(n) => traverse(lts.tail, n, currentNode +: stack)
          case None =>
            val finishTree = lts.tail.foldRight(Option(Node('!', Set())): Option[Node])((nxt, acc) => Option(Node(nxt, acc.toSet))).toList
            traverse(Array[Char](), currentNode.copy(kids = currentNode.kids + Node(nxtChar, finishTree.toSet)), stack)
        }
      }
      else currentNode
    }

    tree match {
      case Nil => chars.foldRight(Option(Node('!', Set())): Option[Node])((nxt, acc) => Option(Node(nxt, acc.toSet))).toList
      case x if x.exists(_.value == chars.head) => x.filter(_.value != chars.head) :+ traverse(chars.tail, x.find(_.value == chars.head).get)
      case _ => tree ++ chars.foldRight(Option(Node('!', Set())): Option[Node])((nxt, acc) => Option(Node(nxt, acc.toSet))).toList
    }

  }

  @tailrec
  def evaluatePwdStr(check: String, lex: List[Node], acc: Seq[String] = Nil): String = {

    @tailrec
    def treeSearcher(ltrs: Array[Char], nxtNodes: Set[Node], index: Int = 1): Option[Int] = {
      if (nxtNodes.exists(_.value == '!')) Option(index)
      else if (nxtNodes.exists(_.value == ltrs.head)) treeSearcher(ltrs.tail, nxtNodes.find(_.value == ltrs.head).map(_.kids).get, index + 1)
      else None
    }

    val chars = check.toCharArray
    val firstNode = lex.find(l => chars.nonEmpty && l.value == chars.head)
    if (check.isEmpty) acc.mkString(" ")
    else if (firstNode.isDefined) {
      val p = treeSearcher(chars.tail, firstNode.get.kids)
      if (p.isEmpty) "WRONG PASSWORD"
      else {
        val pwd = check.substring(0, p.get)
        evaluatePwdStr(check.substring(p.get), lex, acc :+ pwd)
      }
    } else "WRONG PASSWORD"
  }

  def apply(userPwds: String, loginAttempt: String): String = {
    val passTree = userPwds.split(" ").foldLeft(List[Node]())((acc, nxt) => buildFromWord(nxt, acc))
    evaluatePwdStr(loginAttempt, passTree)
  }

}

object TestLex {
  import Lexxie._

  /*
  Input Format

First line contains an integer T, the total number of test cases. Then T test cases follow.
First line of each test case contains N, the number of users with passwords. Second line contains N space separated strings, pass[1] pass[2]
... pass[N], representing the passwords of each user. Third line contains a string, loginAttempt,
for which Yu has to tell whether it will be accepted or not.

Constraints
  • 1 <= T <= 10
  • 1 <= N <= 10
  • pass[i] ≠ pass[j], 1 <= i < j <= N
  • 1 <= length(pass[i]) <= 10, where i E[1,N]
  • 1 < length(loginAttempt) <= 2000
  • loginAttempt and pass[i] contains only lowercase latin characters (a-z)

   */
  def runBenchmarks(): Unit = {
    val lines = IOHelpers.getResourceLines("/input/lexxi1").tail
    lines.sliding(3, 3).foreach( l => {
//      val pt = l.tail.head.split(" ").foldLeft(List[Node]())((acc, nxt) => {
//        buildFromWord(nxt, acc)
//      })
//      val r = stopWatch(evaluatePwdStr(l.last, pt))
      val r = Lexxie(l.tail.head, l.last)
      println(r)
    })

  }


  def main(args: Array[String]): Unit = {
   runBenchmarks()
  }


  def smallTests(): Unit = {
    val w1 = buildFromWord("because")
    val w2 = buildFromWord("better", w1)
    val w3 = buildFromWord("dog", w2)
    println(w3)
  }


  def runSampleTest1(): Unit = {
    Seq(
      ("because can do must we what", "wedowhatwemustbecausewecan"),
      ("hello planet", "helloworld"),
      ("ab abcd cd", "abcd")
    ).foreach { case (pwds, check) =>
      val pt = pwds.split(" ").foldLeft(List[Node]())((acc, nxt) => buildFromWord(nxt, acc))
      println(evaluatePwdStr(check, pt))
    }
  }

  def runSampleTest2(): Unit = {
    val result = Seq(
      ("ozkxyhkcst xvglh hpdnb zfzahm", "zfzahm"),
      ("gurwgrb maqz holpkhqx aowypvopu", "gurwgrb"),
      ("a aa aaa aaaa aaaaa aaaaaa aaaaaaa aaaaaaaa aaaaaaaaa aaaaaaaaaa", "aaaaaaaaaab")
    ).map { case (pwds, check) =>
      val pt = pwds.split(" ").foldLeft(List[Node]())((acc, nxt) => buildFromWord(nxt, acc))
      evaluatePwdStr(check, pt)
    }
    result.foreach(println)
    val expected = Seq("zfzahm","gurwgrb", "WRONG PASSWORD")
    assert(result == expected)
  }

}



