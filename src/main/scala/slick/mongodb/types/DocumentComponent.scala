package slick.mongodb.types


import slick.ast._
import slick.lifted._
import slick.mongodb.{CollectionNode, SubDocNode}
import slick.mongodb.lifted.MongoDriver
import slick.relational.RelationalTableComponent
import slick.util.ConstArray

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context


/**
 * Created by adam on 02.06.15.
 */
trait DocumentComponent extends RelationalTableComponent {
  driver: MongoDriver =>

  case class Id(id:String)
  abstract class Document[A](_documentTag: Tag, _documentName: String)
    extends Table[A](_documentTag, _documentName) {


    def field[T](name: String)(implicit tt: TypedType[T], sh: Shape[_ <: FlatShapeLevel, _, T, ConstColumn[T]]) = column[T](name)

    /**
     * Method for creating an arrays of primitive types and documents
     * we create and array by passing as a parameter result of method field[T] or doc[T]
     * good idea would be to unify arrays into field ex. field[IndexedSeq[Int]]("someName") or field[IndexedSeq[SomeDocument]]("someName")
     */
    def array[E, U](value: E)(implicit sh: Shape[_ <: FlatShapeLevel, E, U, E]): Query[E, U, IndexedSeq] = {
      val shaped = ShapedValue(value, sh).packedValue
      new WrappingQuery[E, U, IndexedSeq](CollectionNode(ElementSymbol(1), shaped.toNode), shaped) //todo shaped.toNode should be wrapped in array node
    }

    /**
     *Collecting symbols from children of star projection. Each child can be of type Select, SubDocNode which is temporary node for representing nested documents and Ref
     */
    def collectSymbols(tag:Tag,sym:TermSymbol) = tag.taggedAs(Ref(sym)).*.toNode.children(0).children.map(x => x match {
      case Select(in,s) => s
      case SubDocNode(t,n,s,tm) => t
      case Ref(s) =>s
      case CollectionNode(t, _) => t
      case StructNode(elements) => elements(0)._1
      case t:TypeMapping => new TermSymbol {
        override def name: String = "t symbol"
      }
    })

    //TODO this looks ugly. Refactor
    // here I should look for solution
    override def toNode = tableTag match {
      case _: BaseTag =>
        val sym = new AnonSymbol
        val y =tableTag.taggedAs(Ref(sym)).*.toNode
        val x =(collectSymbols(tableTag,sym) zip  tableTag.taggedAs(Ref(sym)).*.toNode.children(0).children).force
        val myVal = y match {
          case TypeMapping(p:ProductNode,_,_) => (collectSymbols(tableTag, sym) zip p.children).force
          case TypeMapping(s:SubDocNode,_,_) => ConstArray((s.termSymbol, s))
        }

        TableExpansion(
          sym,
          tableNode,
          SubDocNode(sym,
            StructNode(myVal),
            Ref(sym),
            tableTag.taggedAs(Ref(sym)).*.toNode))
      case t: RefTag => tableNode
    }
  }

  abstract class SubDocument[A](_documentTag: Tag, _documentName: String)
    extends Document[A](_documentTag, _documentName) {

    override def toNode = tableTag match {
      case _: BaseTag => {

        val docNameSymbol = new FieldSymbol(_documentName)(Seq(), UnassignedType)

        /** I encoded Path for nested documents or fields in refTag. */
        val refine = tableTag.taggedAs(Ref(docNameSymbol))
        val symbols = Path.unapply(refine.tableTag.asInstanceOf[RefTag].path.asInstanceOf[PathElement]).get

        /** type of symbol in the end of list list decide if it will be nested Select or
          *SubDocNode for building type in TableNode , Select for projection */
        // todo find a way to remove SubDocNode completely now subDocNodes are removed in RemoveSubDocNodes phase and AddDynamics
         SubDocNode(
           docNameSymbol,
           StructNode((collectSymbols(tableTag, docNameSymbol) zip refine.*.toNode.children(0).children).force) ,
           Path(symbols),refine.*.toNode )
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
              val symbolsFromTag = Path.unapply(b.taggedAs(Ref(new FieldSymbol(tableName)(Seq(), UnassignedType))).tableTag.asInstanceOf[RefTag].path.asInstanceOf[PathElement]).get
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

  abstract class NewTerm
  import slick.mongodb.lifted.MongoDriver.api._

  /** return a table row class using an arbitrary constructor function. */

  def apply[D <: Document[_]](cons: Tag => D)(t: Tag): D = {  // not the best solution with that name because I pass name from table wher method document is called.Maybe I will figure it out.

    /** we take name and tag of document in which method doc is invoked we use it for building correct path */
    //todo find better way to refTag symbols from tag
    val refTag: RefTag = t match {
      case r: RefTag => r
      case b: BaseTag => {
        val name = t.taggedAs(Ref(new AnonSymbol)).tableName
        t.taggedAs(Ref(new FieldSymbol(name)(Seq(), UnassignedType))).tableTag.asInstanceOf[RefTag]
      }
    }

    val getSymbols = Path.unapply(refTag.path.asInstanceOf[PathElement]).get
    cons(new BaseTag {
      base: BaseTag =>
      def taggedAs(path: Node): Document[_] = cons(
        new RefTag(Path(Path.unapply(path.asInstanceOf[PathElement]).get ::: getSymbols)) {
          def taggedAs(path: Node) = {
            base.taggedAs(path)
          }
      })
    })
  }

  /** return a table row class which has a constructor of type (Tag). */
  def apply[D <: Document[_]](t: Tag): D = macro docMacroImpl.apply[D]
}
object docMacroImpl {

  import slick.mongodb.lifted.MongoDriver.api._

  def apply[D <: Document[_]](c: Context)(t: c.Expr[Tag])(implicit e: c.WeakTypeTag[D]): c.Expr[D] = {
    import c.universe._
    val cons = c.Expr[Tag => D](Function(
      List(ValDef(Modifiers(Flag.PARAM), newTermName("tag"), Ident(typeOf[Tag].typeSymbol), EmptyTree)),
      Apply(
        Select(New(TypeTree(e.tpe)), nme.CONSTRUCTOR),
        List(Ident(newTermName("tag")))
      )
    ))
    reify {
      doc.apply[D](cons.splice)(t.splice)
    }
  }
}
