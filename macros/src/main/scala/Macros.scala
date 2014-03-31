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

        val newCtor = q"""def this() = this(List(Some("")))"""

        // It looks like typer sometimes uses positions to decide whether stuff
        // (secondary constructors in this case) typechecks or not (?!!):
        // https://github.com/xeno-by/scala/blob/c74e1325ff1514b1042c959b0b268b3c6bf8d349/src/compiler/scala/tools/nsc/typechecker/Typers.scala#L2932
        //
        // In general, positions are important in getting error messages and debug
        // information right, but maintaining positions is too hard, so macro writers typically don't care.
        //
        // This has never been a problem up until now, but here we're forced to work around
        // by manually setting an artificial position for the secondary constructor to be greater
        // than the position that the default constructor is going to get after macro expansion.
        //
        // We have a few ideas how to fix positions in a principled way in Palladium,
        // but we'll have to see how it goes.
        val defaultCtorPos = c.enclosingPosition
        val newCtorPos = defaultCtorPos.withEnd(defaultCtorPos.endOrPoint + 1).withStart(defaultCtorPos.startOrPoint + 1).withPoint(defaultCtorPos.point + 1)
        val newBody = body :+ atPos(newCtorPos)(newCtor)

        val helloVal   = q"val $valName: $valType = $valDefault"

        q"$mods class $name[..$tparams](..$first, $helloVal)(...$rest) extends ..$parents { $self => ..$newBody }"
        }
      }
    }
    c.Expr[Any](result)
  }
}

class hello extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro helloMacro.impl
}
