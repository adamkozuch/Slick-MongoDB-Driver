package slick.mongodb.lifted

import com.mongodb.casbah.Imports._

import scala.annotation.implicitNotFound
import scala.language.implicitConversions
import slick.ast._
import slick.mongodb.types.doc

trait MongoInsertInvokerComponent { driver: MongoDriver =>
  type InsertInvoker[T] = InsertInvokerDef[T]

  /** Create an InsertInvoker -- this method should be implemented by drivers as needed */
  def createInsertInvoker[T](compiled: CompiledInsert): InsertInvoker[T] = new InsertInvokerDef(compiled)
  override type CompiledInsert = Node     // TODO: change to MongoNode when moving from SQL node tree to mongo tree

  @implicitNotFound("Implicit converter of type ${T}=>DBObject required for MongoDB InsertInvoker")
  final class InsertInvokerDef[T](val node: CompiledInsert) extends GenericLiftedMongoInvoker[T] {
    println(s"Query invoker created with node:\t$node")

    override def queryNode = node

    /** Used to convert specified type to DBObject */
    val binder: Product => MongoDBObject = { p: Product =>
      val coll = attributeNames
      var counter = 0;
      def joinAttributes(p:Any):List[Any] = p.asInstanceOf[Product].productIterator.toList.map(
        x => x match {

          case v:Vector[_] => {
            var s = coll(counter)
            counter = counter + 1
            (s, v)
          }

          case y:Boolean => {
            var s = coll(counter)
            counter = counter + 1
            (s, y)
          }

          case y:Double => {
            var s = coll(counter)
            counter = counter + 1
            (s, y)
          }

          case y:Integer => {
            var s = coll(counter)
            counter = counter + 1
            (s, y)
          }

          case y:Long => {
            var s = coll(counter)
            counter = counter + 1
            (s, y)
          }

          case y:String => {
            var s = coll(counter)
            counter = counter + 1
            (s, y)
          }

          case x:Any => { // assuming that it matches document
            var s = coll(counter)
            counter = counter + 1
            s -> MongoDBObject(joinAttributes(x).asInstanceOf[List[(String, Any)]])
          }
        }
      )

      val valuesWithAttribues = joinAttributes(p)

      MongoDBObject(valuesWithAttribues.asInstanceOf[List[(String, Any)]])
    }

    // TODO: should we use Unit as a write result instead of Mongo driver results?
    type SingleInsertResult = WriteResult
    type MultiInsertResult = BulkWriteResult

    /** Insert a single value */
    def +=(value: T)(implicit session: Backend#Session): SingleInsertResult =
      collection(session).insert(binder(value.asInstanceOf[Product]))

    /** Insert a collection of values */
    def ++=(values: Iterable[T])(implicit session: Backend#Session): MultiInsertResult = {
      val builder = collection(session).initializeOrderedBulkOperation
      for {document <- values} builder.insert(binder(document.asInstanceOf[Product]))
      builder.execute()
    }
  }
}
