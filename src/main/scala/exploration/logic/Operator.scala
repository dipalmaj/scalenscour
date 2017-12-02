package dot.logic

/**
  * Created by wanderer on 4/25/17.
  */

sealed trait Operator {
  def apply(statementP: Boolean, statementQ: Boolean): Boolean

  def toString: String

  def operation: String

  def description: String
}

// Could make this like a tuple design but different to allow for operator
//sealed class EmptyOp
// Could this use assert with catch or is that misusing assert
case object =><= extends Operator {
  override def toString = "<=>"
  override def operation = "biconditional"
  override def description = "P=Q :: Fails on +P,-Q and -P,+Q"
  // Option(s"Left: $statementP != Right: $statementQ")
  override def apply(statementP: Boolean, statementQ: Boolean): Boolean = statementP == statementQ
}

case object >~> extends Operator {
  override def toString = "~>"
  override def operation = "implication"
  override def description = "¬PvQ :: Fails on +P,-Q"
  // Option(s"")
  override def apply(statementP: Boolean, statementQ: Boolean): Boolean = !statementP || statementQ
}

case object <~< extends Operator {
  override def toString = "<~"
  override def operation = "implication"
  override def description = "¬QvP :: Fails on +Q,-P"
  override def apply(statementP: Boolean, statementQ: Boolean): Boolean = !statementQ || statementP
}

case object ~~ extends Operator {
  override def toString = "<<"
  override def operation = "unary"
  override def description = "P :: Fails on -P"
  override def apply(statementP: Boolean, right: Boolean = true): Boolean = !statementP
}