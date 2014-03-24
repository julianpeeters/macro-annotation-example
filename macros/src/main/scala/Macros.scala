package models

import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

import scala.io._

object helloMacro {


  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    import Flag._

    def boxTypeTrees(typeName: String) = {
      val types = typeName.dropRight(typeName.count( c => c == '[')).split('[').map(g => newTypeName(g)).toList  
      val typeTrees: List[Tree] = types.map(t => tq"$t")
      typeTrees.reduceRight((a, b) => tq"$a[$b]")
    }

    val result = {
      annottees.map(_.tree).toList match {

        //Thanks to Eugene Burmako, Den Shabalin, and Travis Brown
        case q"$mods class $name[..$tparams](..$first)(...$rest) extends ..$parents { $self => ..$body }" :: Nil => {

        val valName    = newTermName("x")
        val valType = boxTypeTrees("List[Option[String]]")
        val valDefault = q"""List(Some("foo"))"""

        val helloVal   = q"val $valName: $valType = $valDefault"

        q"$mods class $name[..$tparams](..$first, $helloVal)(...$rest) extends ..$parents { $self => ..$body }"
        }
      }
    }
    c.Expr[Any](result)
  }
}

class hello extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro helloMacro.impl
}
