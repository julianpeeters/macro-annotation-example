package models

import scala.reflect.macros.Context
import scala.language.experimental.macros
//import scala.annotation.StaticAnnotation

object TypeMatcher {

  def matchType(typeName: String, c: Context) {
    import c.universe._
    import Flag._

    val card = typeName.count(c => c == '[')
  //  val d = card match {
    //  case 1 => {
val Array(sp, ps) = typeName.dropRight(1).split('[').map(g => newTypeName(g)); println(sp + ps.toString); tq"$sp[$ps]"//}
      //case _ => error("TypeMatcher expected a parameterized type but was given none apparently")
   // }

//println(d)
//d
  }
}
