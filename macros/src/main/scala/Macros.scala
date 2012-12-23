package org.i5y.mongomacros

import scala.reflect.macros.Context
import scala.language.experimental.macros

case class Q[A](query: String) {
  def eql(t: A) = (query, t)
}

object Macros {

  def createHelper[T] = new Helper[T]

  class Helper[T] {
    def apply(query: String): Q[_] = macro Macros.que_impl2[T]
  }

  def que_impl2[O: c.WeakTypeTag](c: Context)(query: c.Expr[String]): c.universe.Expr[Q[_]] = {
    import c.universe._

    val queryString = query.tree match {
      case (Literal(Constant(queryString: String))) => queryString
      case _ => c.abort(c.enclosingPosition, "query isn't a string literal")
    }

    val objectType = c.weakTypeOf[O]

    val parts = queryString.split("\\.")

    val ft = parts.foldLeft(objectType) {
      case (oT, path) =>
        val fieldMember = oT.member(newTermName(path)) orElse {
          c.abort(c.enclosingPosition, s"The property $path isn't a field of $oT")
        }

        val fieldMemberType = fieldMember.typeSignatureIn(oT) match {
          case NullaryMethodType(tpe) => tpe
          case _ => c.abort(c.enclosingPosition, s"$path isn't a field, it must be another thing")
        }
        fieldMemberType
    }

    def genRes[T: WeakTypeTag] = reify {
      Q.apply[T](query.splice)
    }
    genRes(c.WeakTypeTag(ft))
  }
}