package org.hammerlab.test.implicits

import scala.annotation.{ StaticAnnotation, compileTimeOnly }
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

/**
 * Annotate a trait with [[Conversions[From, To]]] this to generate a bunch of handy implicits for using one type
 * ([[From]]) interchangeably with another ([[To]]) in tests:
 *
 * \@Conversions[Int, String]
 * trait IntToStringConversions {
 *   // Implicitly convert integers to strings by concatenating their string representations to themselves.
 *   implicit def intToString(n: Int): String = s"$n$n"
 * }
 *
 * The annotation will unroll a bunch of implicit functions for converting from e.g. [[Option[Int]]] to
 * [[Option[String]]], [[Seq[Int]]] to [[Seq[String]]], etc. If an implicit [[Int => String]] is not give in the body of
 * the trait, then one needs to be in scope where it is declared.
 *
 * Downstream classes can then mix-in `IntToStringConversions` to have all such implicits in scope.
 *
 * A macro is used so that multiple different [[Conversions]] traits can be mixed-in to a given concrete class without
 * the implicit-names colliding (which would otherwise result in all but the last-to-be-mixed-in being hidden/shadowed);
 * instead, the macro seeds all implicits' names with the types being converted between, avoiding the issue.
 *
 * In order to use the [[Conversions]] annotation, macro paradise must be enabled.
 */
@compileTimeOnly("enable macro paradise to expand macro annotations")
class Conversions[From, To] extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro Conversions.impl
}

object Conversions {
  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val inputs = annottees.map(_.tree).toList

    val Apply(
      Select(
        Apply(
          Select(
            New(
              AppliedTypeTree(
                _,
                List(
                  Ident(tt),
                  Ident(ut)
                )
              )
            ),
            _
          ),
          _
        ),
        _
      ),
      _
    ) = c.macroApplication

    inputs match {
      case (param: ClassDef) :: rest ⇒

        val q"$_ trait $name { ..$stats }" = param

        val T = tt.toTypeName
        val U = ut.toTypeName

        def m(name: String) = TermName(s"${name}_${T}_$U")

        val expr =
          c.Expr[Any](
            q"""
              trait $name {
                ..$stats

                import scala.reflect.ClassTag
                private val f = implicitly[$T => $U]
                implicit def ${m("convertOpt")}(t: Some[$T]): Option[$U] = t.map(f)
                implicit def ${m("convertTuple2Keys")}[V](t: ($T, V)): ($U, V) = (t._1, t._2)
                implicit def ${m("convertMapKeys")}[V](m: Map[$T, V]): Map[$U, V] = m.map(t ⇒ (f(t._1), t._2))
                implicit def ${m("toTupleList")}[V](s: Seq[($T, V)]): List[($U, V)] = s.map(t ⇒ (f(t._1), t._2)).toList
                implicit def ${m("toTupleArray")}[V: ClassTag](s: Array[($T, V)])(implicit ct: ClassTag[$U]): Array[($U, V)] = s.map(t ⇒ (f(t._1), t._2))
                implicit def ${m("toSeq")}(s: Seq[$T]): Seq[$U] = s.map(f)
                implicit def ${m("toVector")}(s: Seq[$T]): Vector[$U] = s.map(f).toVector
                implicit def ${m("toList")}(s: Seq[$T]): List[$U] = s.map(f).toList
                implicit def ${m("toArray")}(s: Seq[$T])(implicit ct: ClassTag[$U]): Array[$U] = s.map(f).toArray
                implicit def ${m("convertArray")}(s: Array[$T])(implicit ct: ClassTag[$U]): Array[$U] = s.map(f)
              }
             """
          )

        c.Expr[Any](
          Block(
            expr.tree :: rest,
            Literal(Constant(()))
          )
        )
      case _ ⇒
        throw new Exception(s"expected ClassDef: $inputs")
    }
  }
}
