package dot.logic

/**
  * Created by wanderer on 4/25/17.
  */
sealed trait Statements

sealed class State extends Statements {
  def ::>[T](c: T => Boolean): Statement[T] = Statement[T](c)
}

final case class Statement[T](q: T => Boolean) extends Statements {
  // TODO better way to do this?
  def <~(p: T => Boolean): StatementEval[T] = StatementEval[T](<~<, q, p)
  def ~>(p: T => Boolean): StatementEval[T] = StatementEval[T](>~>, q, p)
  def <=>(p: T => Boolean): StatementEval[T] = StatementEval[T](=><=, q, p)
}

sealed trait StatementEvaluators[T] {
  def q: T => Boolean
  def p: T => Boolean
  def operator: Operator

  def apply(value: T): Boolean = {
    val qResult = q(value)
    println(value)
    println(qResult)
    operator(qResult, p(value))
  }
}

final case class StatementEval[T](operator: Operator, q: T => Boolean, p: T => Boolean) extends StatementEvaluators[T]

