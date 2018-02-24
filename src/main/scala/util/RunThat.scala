package util

import scala.reflect.runtime.universe

object RunThat {

  def main(args: Array[String]): Unit = {

    val rm = universe.runtimeMirror(getClass.getClassLoader)
    val obj = rm.staticModule(args.head)
    val im = rm.reflectModule(obj)
    val objMirror = rm.reflect(im.instance)
    val method = im.symbol.info.decl(universe.TermName("main")).asMethod

    objMirror.reflectMethod(method)(args.tail)


  }

}
