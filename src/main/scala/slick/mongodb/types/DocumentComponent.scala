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

  }
  object Document {
    @inline implicit final def documentShape[Level >: FlatShapeLevel <: ShapeLevel, T, C <: Document[_]](implicit ev: C <:< Document[T]) = RepShape[Level, C, T]
  }
}
// todo It would be better to mixin doc into DocumentComponent(macro needs to be in static context)
// probably shouldn't use Document[_] from api
object doc {
    import slick.mongodb.lifted.MongoDriver.api._
  /** return a table row class using an arbitrary constructor function. */
  def apply[D <: Document[_]](cons: Tag => D): D = {
    cons(new BaseTag {
      base =>
      def taggedAs(path: Node): Document[_] = cons(new RefTag(path) {
        def taggedAs(path: Node) = base.taggedAs(path)
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













