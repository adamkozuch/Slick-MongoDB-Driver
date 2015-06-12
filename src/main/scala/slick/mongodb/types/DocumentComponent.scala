package slick.mongodb.types

import slick.ast._
import slick.collection.heterogeneous.{TypedFunction2, HList}
import slick.lifted._
import slick.mongodb.lifted.MongoDriver
import slick.profile.{RelationalDriver, RelationalTableComponent}

import scala.reflect.ClassTag


/**
 * Created by adam on 02.06.15.
 */
trait DocumentComponent extends RelationalTableComponent {
  driver: MongoDriver =>

  abstract class Document[A](_documentTag: Tag, _documentName: String)
    extends Table[A](_documentTag, _documentName) {

    def matchType[T](a: String): Rep[T] = implicitly[ClassTag[T]] match {
      case Document => Document[T](a)
      case ScalaBaseType => column[T](a)
    }
  }

  abstract case class Document2[A, B](_documentTag: Tag, _documentName: String) extends Document[A](_documentTag, _documentName)

  abstract case class Document3[A, B, C](_documentTag: Tag, _documentName: String) extends Document[A](_documentTag, _documentName)


  object Document {
    def apply[A](a: String)(implicit _documentTag: Tag, _documentName: String) = new Document[A](_documentTag, _documentName) {
      def cA = matchType[A](a) //type A can be mix of documents and multiple primitive types but how then match a projection
      def * = cA //maybe we should make Shape that takes multiple type parameters ProvenShape[A], ProvenShape[A,B], ProvenShape[A,B,C], ProvenShape[A,B,C,D] etc
    } //I should not creating an object but defining

    def apply[A, B](a: String, b: String)(implicit _tableTag: Tag, _tableName: String) = new Document2[A, B](_tableTag, _tableName) {
      def cA = matchType[A](a)
      def cB = matchType[B](b)
      def * = (cA, cB) //ProvenShape[A,B]
    }

    def apply[A, B,C](a: String, b: String,c:String)(implicit _tableTag: Tag, _tableName: String) = new Document3[A, B,C](_tableTag, _tableName) {
      def cA = matchType[A](a)
      def cB = matchType[B](b)
      def cC = matchType[C](c)
      def * = (cA, cB,cC)  //ProvenShape[A,B,C]
    }

  }

  /*
  What I wanna achieve
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

}




