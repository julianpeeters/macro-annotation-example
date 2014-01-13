package models

import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

import scala.io._

object helloMacro {



  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    import Flag._

    def makeDefault(valDef: ValDef) = valDef match {
      case ValDef(mods, name, tpt, rhs) => ValDef(
        Modifiers(
          mods.flags | DEFAULTPARAM, mods.privateWithin, mods.annotations
        ),
        name, tpt, rhs
      )
    }

    val result = {
      annottees.map(_.tree).toList match {
        //Thanks to Eugene Burmako, Den Shabalin, and Travis Brown
        case q"$mods class $name[..$tparams](..$first)(...$rest) extends ..$parents { $self => ..$body }" :: Nil =>

          val valName    = newTermName("x")
          val valType    = tq"String"
          val valDefault = q""""foo""""

          val helloVal   = makeDefault(q"val $valName: $valType = $valDefault")
          val vals = List(helloVal)

          q"$mods class $name[..$tparams](..$vals)(...$rest) extends ..$parents { $self => ..$body }"
      }
    }
    c.Expr[Any](result)
  }
}

class hello extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro helloMacro.impl
}
