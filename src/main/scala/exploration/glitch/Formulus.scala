package exploration.glitch

import exploration.datastructure.AForm


/**
  * Created by wanderer on 4/25/17.
  */
object Formulus {
  def main(args: Array[String]) = {

    import AForm.++

    val oprs: AForm[Int] = AForm(1,2,3,4,5)
    val nops: AForm[Int] = AForm (9,2,1)

    val dbl: AForm[AForm[Int]] = AForm(nops, oprs)
    println(oprs)
   println(oprs.tail)
    println(AForm.setHead(oprs, 2))

    println(AForm.drop(oprs, 2))
    println(AForm.reverse(oprs))
    println(AForm.append(oprs, 7))
    println(++(oprs,nops))
    println(AForm.length(oprs))
    println(AForm.lengthR(oprs))
    println(AForm.bump(oprs))
    println(AForm.map(oprs)(x => x * 100))

    println(AForm.flatMap(dbl)(x => AForm.map(x)( x => x + 25)))

    println(AForm.filter(oprs)(_ % 2 == 0))

  }

}
