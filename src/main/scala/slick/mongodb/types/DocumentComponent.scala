package slick.mongodb.types

import slick.ast.Type.Scope
import slick.ast._
import slick.lifted._
import slick.mongodb.SubDocNode
import slick.mongodb.lifted.MongoDriver
import slick.profile.RelationalTableComponent
import scala.collection.mutable.ListBuffer
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context


/**
 * Created by adam on 02.06.15.
 */
trait DocumentComponent extends RelationalTableComponent {
  driver: MongoDriver =>
  class NewAnonSymbol extends AnonSymbol
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
      new WrappingQuery[E, U, IndexedSeq](shaped.toNode, shaped) //todo shaped.toNode should be wrapped in array node
    }

    override def toNode = tableTag match {
      case _: BaseTag =>

        val sym = new NewAnonSymbol

        val collectSymbols = tableTag.taggedAs(Ref(sym)).*.toNode.children(0).children.map(x => x match{
          case Select(in,s) => s
          case SubDocNode(t,n) => t
        })

        // product node is changed during query compilation
        TableExpansion(sym, tableNode, StructNode((collectSymbols zip  tableTag.taggedAs(Ref(sym)).*.toNode.children(0).children ).toIndexedSeq))
      case t: RefTag => t.path
    }

    //todo try to make it work
   // def doc[D <: slick.mongodb.lifted.MongoDriver.api.Document[_]](t:Tag = tableTag, name:String= tableName)  = macro docMacroImpl.apply[D]
  }

  abstract class SubDocument[A](_documentTag: Tag, _documentName: String)
    extends Document[A](_documentTag, _documentName) {

    override def toNode = tableTag match {
      case _: BaseTag => {



        val docNameSymbol = new FieldSymbol(_documentName)(Seq(), UnassignedType)

        val collectSymbols = tableTag.taggedAs(Ref(docNameSymbol)).*.toNode.children(0).children.map(x => x match{
          case Select(in,s) => s
          case StructNode(ls) =>ls(0)._1// hack that works
          case SubDocNode(t,n) => t
          case p :Pure => new FieldSymbol("symbolkolekcji")(Seq(), UnassignedType)
        })


        val refine = tableTag.taggedAs(Ref(docNameSymbol))
        val symbols = Path.unapply(refine.tableTag.asInstanceOf[RefTag].path).get

        /** type of symbol in the end of list list decide if it will be nested Select or
          *StructNode for building type in TableNode , Select for projection */
         // todo I should put symbol not from this table but from next

        val result = symbols match {
          case l if l.last.isInstanceOf[NewAnonSymbol] => SubDocNode(docNameSymbol,StructNode(  (collectSymbols zip refine.*.toNode.children(0).children).toIndexedSeq))
          case l if l.last.isInstanceOf[AnonSymbol] => Path(l)
        }

        result
      }
      case t: RefTag => t.path // when I will need that refTag
    }

    override def column[C](n: String, options: ColumnOption[C]*)(implicit tt: TypedType[C]): Rep[C] = {
      if (tt == null) throw new NullPointerException(
        "implicit TypedType[C] for column[C] is null. " +
          "This may be an initialization order problem. " +
          "When using a MappedColumnType, you may want to change it from a val to a lazy val or def.")
      new Rep.TypedRep[C] {

        override def toNode =
          tableTag match {
            case r: RefTag => Select(r.path, FieldSymbol(n)(options, tt)) :@ tt
            case b: BaseTag => {
              val symbolsFromTag = Path.unapply(b.taggedAs(Ref(new FieldSymbol(tableName)(Seq(), UnassignedType))).tableTag.asInstanceOf[RefTag].path).get
              Path(List(FieldSymbol(n)(options, tt)) ::: symbolsFromTag) :@ tt
            }
          }
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

  def apply[D <: Document[_]](cons: Tag => D)(t: Tag, name: String): D = {  // not the best solution with that name because I pass name from table wher method document is called.Maybe I will figure it out.

    /** we take name and tag of document in which method doc is invoked we use it for building correct path */
    //todo find solution that do not require tag and name
    //todo find better way to extract symbols from tag
    val extract: RefTag = t match {
      case r: RefTag => r
      case b: BaseTag => {
        t.taggedAs(Ref(new FieldSymbol(name)(Seq(), UnassignedType))).tableTag.asInstanceOf[RefTag]
      }// extracting ref tag
    }

    val getSymbols = Path.unapply(extract.path).get
    cons(new BaseTag {
      base: BaseTag =>
      def taggedAs(path: Node): Document[_] = cons(new RefTag(Path(Path.unapply(path).get ::: getSymbols)) {
        def taggedAs(path: Node) = {
          base.taggedAs(path)
        }
      })
    })
  }

  /** return a table row class which has a constructor of type (Tag). */
  def apply[D <: Document[_]](t: Tag, name: String): D = macro docMacroImpl.apply[D]
}

object docMacroImpl {

  import slick.mongodb.lifted.MongoDriver.api._

  def apply[D <: Document[_]](c: Context)(t: c.Expr[Tag], name: c.Expr[String])(implicit e: c.WeakTypeTag[D]): c.Expr[D] = {
    import c.universe._
    val cons = c.Expr[Tag => D](Function(
      List(ValDef(Modifiers(Flag.PARAM), newTermName("tag"), Ident(typeOf[Tag].typeSymbol), EmptyTree)),
      Apply(
        Select(New(TypeTree(e.tpe)), nme.CONSTRUCTOR),
        List(Ident(newTermName("tag")))
      )
    ))
    reify {
      doc.apply[D](cons.splice)(t.splice, name.splice)
    }
  }
}













