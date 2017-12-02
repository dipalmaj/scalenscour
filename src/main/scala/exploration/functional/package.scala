package dot

package object functional {
  class FnWrapper[T1, T2, R](fn : (T1, T2) => R) {
    def apply(a1 : T1, a2 : T2) = fn(a1, a2)
  }

  //  case class AppendFn[L1 <: Form, L2 <: Form](fn : (L1, L2) => L1#Append[L2]) extends FnWrapper(fn)
  // case class AppendFn[L1 <: Expression, L2 <: Expression](fn : (L1, L2) => L1#Append[L2]) extends FnWrapper(fn)
  //  def append[L1 <: Expression, L2 <: Expression](l1: L1, l2: L2)(implicit fn: AppendFn[L1,L2]): L1#Append[L2] = fn(l1, l2)
}