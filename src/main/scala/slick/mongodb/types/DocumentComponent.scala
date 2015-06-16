package slick.mongodb.types

import slick.lifted._
import slick.mongodb.lifted.MongoDriver
import slick.profile.RelationalTableComponent


/**
 * Created by adam on 02.06.15.
 */
trait DocumentComponent extends RelationalTableComponent {
  driver: MongoDriver =>

  abstract class Document[A](_documentTag: Tag, _documentName: String)
    extends Table[A](_documentTag, _documentName) {

    def document[T](name: String)(x: T)(implicit ol: OptionLift[T, Rep[Option[T]]]): Rep[Option[T]] = ol.lift(x)

  }
}
  /*
  What want to achieve
    class Suppliers(tag: Tag)
      extends Document[Int,  Document[String, Document[String,Int]], String, ](tag, "SUPPLIERS") { //it doesn't take one type parameter but couple

      // This is the primary key column:
      def id: Rep[Int] = column[Int]("SUP_ID", O.PrimaryKey)
      def additionalData:Rep[Option[Rep[String], Rep[Option[Rep[String],Rep[Int]] =
      Document("data", (surname:String, Document("competitors",(compName:String,rank:Int ))))     // we use here companion object
      def city: Rep[String] = column[String]("CITY")


      //compiler should demand (Int,Option[(String,Option[(String,Int)])],String)
      def *  = (id, additionalData ,city)
    }
   */











