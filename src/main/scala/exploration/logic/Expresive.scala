package dot.logic

/**
  * Created by wanderer on 4/26/17.
  */
object Expresive {

  def main(args: Array[String]): Unit = {
    val p: (Int => Boolean) = _ > 3
    val q: (Int => Boolean) = _ > 5

    val statmkr = new State

    val state = statmkr ::> q <~ p
    val states = statmkr ::> q <=> p

    val frm = new FormNil[Int]
//    val firstStatement = frm <<= state

    val form = (frm === state) <&&> states <||> state

    println(form.head(2))
    println(form.tail.head(2))

//    val r = Form(state, states)

  }

}
