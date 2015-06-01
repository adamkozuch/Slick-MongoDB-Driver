package slick.mongodb.lifted


import com.mongodb.casbah.Imports._

import scala.annotation.implicitNotFound
import scala.language.implicitConversions
import slick.ast._
import slick.profile.BasicInsertInvokerComponent

trait MongoInsertInvokerComponent extends BasicInsertInvokerComponent{ driver: MongoDriver =>
  override type InsertInvoker[T] = InsertInvokerDef[T]

  /** Create an InsertInvoker -- this method should be implemented by drivers as needed */
  override def createInsertInvoker[T](compiled: CompiledInsert): InsertInvoker[T] = new InsertInvokerDef(compiled)
  override type CompiledInsert = Node     // TODO: change to MongoNode when moving from SQL node tree to mongo tree

  // TODO: use mongo-specific nodes, add support for nested document structure
  @implicitNotFound("Implicit converter of type ${T}=>DBObject required for MongoDB InsertInvoker")
  final class InsertInvokerDef[T](val node: CompiledInsert) extends super.InsertInvokerDef[T] with GenericLiftedMongoInvoker[T] {
    println(s"Query invoker created with node:\t$node")

    override def queryNode = node

    /** Used to convert specified type to DBObject */
    val binder: Product => MongoDBObject = { p: Product =>
      val pairs: List[(String, Any)] = (attributeNames.iterator zip p.productIterator).toList
      MongoDBObject(pairs)
    }

    // TODO: should we use Unit as a write result instead of Mongo driver results?
    override type SingleInsertResult = WriteResult
    override type MultiInsertResult = BulkWriteResult

    /** Insert a single value */
    override def +=(value: T)(implicit session: Backend#Session): SingleInsertResult =
      collection(session).insert(binder(value.asInstanceOf[Product]))

    /** Insert a collection of values */
    override def ++=(values: Iterable[T])(implicit session: Backend#Session): MultiInsertResult = {
      val builder = collection(session).initializeOrderedBulkOperation
      for {document <- values} builder.insert(binder(document.asInstanceOf[Product]))
      builder.execute()
    }
  }
}
