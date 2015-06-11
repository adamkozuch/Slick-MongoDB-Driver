package slick.mongodb.types

import slick.ast._
import slick.lifted._
import slick.mongodb.lifted.MongoDriver
import slick.profile.{RelationalDriver, RelationalTableComponent}


/**
 * Created by adam on 02.06.15.
 */
trait DocumentComponent extends RelationalTableComponent {
  driver: MongoDriver =>

  // lazy val documentNode = DocumentNode(schemaName, tableName, tableIdentitySymbol, this, tableIdentitySymbol)

  // when we have array we just check if its elements are of the same type

    abstract class Document[A](_tableTag: Tag, _tableName: String)  // T to jest właściwie cała wartośc wyrażeniua
      extends Table[A](_tableTag,_tableName)
    {
       type G <: Seq[Object]
    override  def tableProvider: RelationalDriver = driver

     override def tableIdentitySymbol: TableIdentitySymbol = SimpleTableIdentitySymbol(driver, schemaName.getOrElse("_"), tableName)

     override val O: driver.columnOptions.type = columnOptions

      type E <: AnyRef
      type B <: AnyVal


    //  implicitly[Shape[_, Rep[Option[(Rep[Int], Rep[String])]], _, _]]

//      def document[C](key: String)(implicit tt:TypedType[C]):Rep[Option[C]] = tt match  // method should return nested lists ex. List(Rep,List(Rep,Rep))
//      {
//      //  case a:B => Rep.Some[Rep[C],Rep[Option[C]]](column[C](key))//opcja jest typem any
//        case a:B =>Rep.Some[Rep[C],Rep[Option[C]]](column[C](key))(OptionLift.repOptionLift[Rep[C],C])
//       // case a:List => Rep.Some[Rep[C],Rep[Option[C]]](a.map(x => Rep.Some[Rep[C],Rep[Option[C]]]))
//
//
//
//      }

       // we can have multiple columns in one list probably I should use map or foreach
      }



  //    when we define schema we use columns or DocumentObject
    }

  //może zaniast sekwencji dokumentów  powinna byc sekwencja List[Any]
  //abstract case class DSeq[T](elems:List[Any])(_tableTag: Tag, collectionName: String) extends  Document(_tableTag: Tag, collectionName: String)
  //lists later
  //  abstract case class DocumentObj(_tableTag: Tag, collectionName: String) //probably I dont need schema
  ////                                 (bindings: Map[String,V]) extends Document(_tableTag: Tag, collectionName: String) // czym są te bindingi
  //  abstract case class DocumentRep(r:String)(_tableTag: Tag, collectionName: String)
  //    extends Document(_tableTag: Tag, collectionName: String)



