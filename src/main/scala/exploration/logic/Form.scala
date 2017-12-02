package dot.logic



/**
  * Created by wanderer on 4/25/17.
  */
//
//sealed trait Form
//
//final case class FormCons[S, T <: Form](head: S, tail: T) extends Form {
//def <&&>[A](v: S) = FormCons(v, this)
//def <||>[A](v: S) = FormCons(v, this)
//}
//
//sealed class FormNil extends Form {
//  def <&&>[S](v: S) = FormCons(v, this)
//  def ===[S](v: S) = FormCons(v, this)
//}
//
//case class FormEval[A](e: Form) extends Form {
////  val fNil = new FormNil
////  def <&&>[B](v: B) = FormCons(v, fNil <&&> e)
//}
//
//
import dot.logic._

import scala.language.higherKinds

/**
  * Created by wanderer on 4/25/17.
  */

sealed trait Forms {
  type S
}

final case class FormCons[H, T <: Forms](head: H, tail: T) extends Forms {
  type S = H
  def <&&>(v: S) = FormCons(v, this)
  def <||>(v: S) = FormCons(v, this)
}

sealed class FormNil[T] extends Forms {
  type S = StatementEval[T]

  def <&&>(v: S) = FormCons(v, this)
  def <||>(v: S) = FormCons(v, this)
  def ===(v: S) = FormCons(v, this)
}

case class Form[A](e: StatementEval[A]) extends Forms

