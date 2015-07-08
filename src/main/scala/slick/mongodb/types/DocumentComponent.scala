package slick.mongodb.types

import slick.ast._
import slick.lifted._
import slick.mongodb.lifted.MongoDriver
import slick.profile.RelationalTableComponent
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context


/**
 * Created by adam on 02.06.15.
 */
trait DocumentComponent extends RelationalTableComponent {
  driver: MongoDriver =>

  abstract class Document[A](_documentTag: Tag, _documentName: String)
    extends Table[A](_documentTag, _documentName) {

    def field[T](name: String)(implicit tt: TypedType[T]) = column[T](name)

    /**
     * temporary method for constructing arrays in the future it will be unified with field
     */
    def array[E, U, R](value: E)(implicit sh: Shape[_ <: FlatShapeLevel, E, U, R]): Query[R, U, IndexedSeq] = {
      val shaped = ShapedValue(value, sh).packedValue
      new WrappingQuery[R, U, IndexedSeq](shaped.toNode, shaped)
    }

    /**
     * implicit conversions created in order to unify arrays in field
     */
//    def prototypeField[T](name: String)(implicit sh: Shape[FlatShapeLevel,_,T,_])= sh
//    implicit def convert2array[T<:IndexedSeq[_],Q<:QueryBase](implicit sh: Shape[FlatShapeLevel,Q,T,_]):Q= ???
//
//    implicit def convert2primitive[T<:TypedType[_],M](implicit sh: Shape[FlatShapeLevel, M,T,_], tt:TypedType[T]):M = ???
//
//    implicit def convert2document[T<:Document[_]](implicit sh: Shape[FlatShapeLevel, _,T,_]):T = ???

    /**
     * copy of query shape with changed ShapeLevel to Flat. It is used because query do not accept NestedShapeLevel
     */
    implicit def flatQueryShape[Level >: FlatShapeLevel <: ShapeLevel, T, Q <: QueryBase[_]](implicit ev: Q <:< Rep[T]) = RepShape[Level, Q, T]

  }

  object Document {
    implicit final def documentShape[Level >: FlatShapeLevel <: ShapeLevel, T, C <: Document[_]](implicit ev: C <:< Document[T]) = RepShape[Level, C, T]

  }
}
object doc {
    import slick.mongodb.lifted.MongoDriver.api._
  /** return a table row class using an arbitrary constructor function. */
  def apply[D <: Document[_]](cons: Tag => D): D = {
    cons(new BaseTag {
      base =>
      def taggedAs(path: Node): Document[_] = cons(new RefTag(path) {
        def taggedAs(path: Node) ={
          base.taggedAs(path) }   //todo use name from binding
      })
    })
  }
  /** return a table row class which has a constructor of type (Tag). */
  def apply[D <: Document[_]]: D = macro docMacroImpl.apply[D]
}

object docMacroImpl {
  import slick.mongodb.lifted.MongoDriver.api._
  def apply[D <: Document[_]](c:  Context)(implicit e: c.WeakTypeTag[D]): c.Expr[D] = {
    import c.universe._
    val cons = c.Expr[Tag => D](Function(
      List(ValDef(Modifiers(Flag.PARAM), newTermName("tag"), Ident(typeOf[Tag].typeSymbol), EmptyTree)),
      Apply(
        Select(New(TypeTree(e.tpe)), nme.CONSTRUCTOR),
        List(Ident(newTermName("tag")))
      )
    ))
    reify {
      doc.apply[D](cons.splice)
    }
  }
}













