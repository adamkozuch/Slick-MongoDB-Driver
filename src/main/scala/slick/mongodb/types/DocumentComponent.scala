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

    def field[T](name: String)(implicit tt: TypedType[T], sh: Shape[_ <: FlatShapeLevel, _, T, _]) = column[T](name)

    /**
     * Method for creating an arrays of primitive types and documents
     * we create and array by passing as a parameter result of method field[T] or doc[T]
     * good idea would be to unify arrays into field ex. field[IndexedSeq[Int]]("someName") orfield[IndexedSeq[SomeDocument]]("someName")
     */
    def array[E, U](value: E)(implicit sh: Shape[_ <: FlatShapeLevel, E, U, E]): Query[E, U, IndexedSeq] = {
      val shaped = ShapedValue(value, sh).packedValue
      new WrappingQuery[E, U, IndexedSeq](shaped.toNode, shaped)
    }
  }

  abstract class SubDocument[A](_documentTag: Tag, _documentName: String)
    extends Document[A](_documentTag, _documentName) {
    override def toNode = tableTag match {
      case _: BaseTag =>
        val sym = new AnonSymbol // todo what would be the right symbol for this
        StructNode(tableTag.taggedAs(Ref(sym)).*.toNode.children(0).children.map(x => (sym, x)).toIndexedSeq)
      case t: RefTag => t.path
    }

    // todo modyfy to node method in a way it will return method with right reference
    override def column[C](n: String, options: ColumnOption[C]*)(implicit tt: TypedType[C]): Rep[C] = {
      if(tt == null) throw new NullPointerException(
        "implicit TypedType[C] for column[C] is null. "+
          "This may be an initialization order problem. "+
          "When using a MappedColumnType, you may want to change it from a val to a lazy val or def.")
      new Rep.TypedRep[C] {
        override def toNode =
          Select((tableTag match {
            case r: RefTag => r.path
            case _ => tableNode
          }), FieldSymbol(n)(options, tt)) :@ tt
        override def toString = (tableTag match {
          case r: RefTag => "(" + _documentName + " " + r.path + ")"
          case _ => _documentName
        }) + "." + n
      }
    }
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













