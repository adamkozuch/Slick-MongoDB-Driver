package slick.mongodb.direct

import com.mongodb.casbah.Imports._
import com.mongodb.casbah.MongoCollectionBase
import com.mongodb.casbah.TypeImports._
import com.mongodb.casbah.commons.TypeImports._
import com.mongodb.{DBCollection, DBCursor}

import scala.language.implicitConversions

//TODO: check if R can be covariant
// TODO: check if we need to move implicit GetResult parameter to findOneTyped method declarations
// TODO: documentation
class TypedMongoCollection[R](val mongoCollection:MongoCollection ,val converter:GetResult[R]) extends MongoCollectionBase {
  def this(collectionName:String)(implicit session: MongoBackend#Session, converter:GetResult[R]) = this(session.collectionByName(collectionName),converter)

  /**
   * Removes objects from the database collection.
   * @param concern WriteConcern for this operation
   *                TODO - Wrapper for WriteResult?
   */
    //todo same method in MongoCollectionBase probably can be removed
//  def remove[A](concern: com.mongodb.WriteConcern = getWriteConcern)(implicit dbObjView: A => DBObject,
//                                                                           encoder: DBEncoder = customEncoderFactory.map(_.create).orNull): WriteResult =
//    underlying.remove(new BasicDBObject, concern, encoder)




  /**
   * Returns a single object converted to Scala type from this collection.
   * @return (Option[R]) Some() of the object found, or <code>None</code> if this collection is empty
   */
  def findOneTyped() = findOne().map(x=>converter(x))

  /**
   * Returns a single object converted to Scala type from this collection matching the query.
   * @param o the query object
   * @return (Option[T]) Some() of the object found, or <code>None</code> if no such object exists
   */
  def findOneTyped[A <% DBObject](o: A) =
    typed(findOne(o: DBObject))


  /**
   * Returns a single object converted to Scala type from this collection matching the query.
   * @param o the query object
   * @param fields fields to return
   * @return (Option[T]) Some() of the object found, or <code>None</code> if no such object exists
   */
  // TODO: implement - issue with read preference
  def findOneTyped[A <% DBObject, B <% DBObject](o: A, fields: B, readPrefs: ReadPreference = getReadPreference) = ???
//    typed(findOne(o: DBObject, fields, readPrefs))

  /**
   * Find an object converted to Scala type by its ID.
   * Finds an object by its id. This compares the passed in
   * value to the _id field of the document.
   *
   * Returns a single object from this collection matching the query.
   * @param id the id to match
   * @return (Option[T]) Some() of the object found, or <code>None</code> if no such object exists
   */
  def findOneByIDTyped(id: AnyRef) = typed(findOneByID(id))

  /**
   * Find an object converted to Scala type by its ID.
   * Finds an object by its id. This compares the passed in
   * value to the _id field of the document.
   *
   * Returns a single object from this collection matching the query.
   *
   * @param id the id to match
   * @param fields fields to return
   * @return (Option[T]) Some() of the object found, or <code>None</code> if no such object exists
   */
  def findOneByIDTyped[B <% DBObject](id: AnyRef, fields: B) = typed(findOneByID(id, fields))

  /**
   * Finds the first document converted to Scala type in the query (sorted) and updates it.
   * If remove is specified it will be removed. If new is specified then the updated
   * document will be returned, otherwise the old document is returned (or it would be lost forever).
   * You can also specify the fields to return in the document, optionally.
   * @return (Option[T]) of the the found document (before, or after the update)
   */
  def findAndModifyTyped[A <% DBObject, B <% DBObject](query: A, update: B) =
    typed(findAndModify(query, update))

  /**
   * Finds the first document converted to Scala type in the query (sorted) and updates it.
   * @return the old document
   */
  def findAndModifyTyped[A <% DBObject, B <% DBObject, C <% DBObject](query: A, sort: B, update: C) =
    typed(findAndModify(query, sort, update))

  /**
   * Finds the first document converted to Scala type in the query and updates it.
   * @return the old document
   */
  def findAndModifyTyped[A <% DBObject, B <% DBObject, C <% DBObject, D <% DBObject](query: A, fields: B, sort: C,
                                                                                remove: Boolean, update: D,
                                                                                returnNew: Boolean, upsert: Boolean) =
    typed(findAndModify(query, fields, sort, remove, update, returnNew, upsert))

  /**
   * Finds the first document converted to Scala type in the query and removes it.
   * @return the removed document
   */
  def findAndRemoveTyped[A <% DBObject](query: A) = typed(findAndRemove(query))
  
  private def typed(dbObject: Option[T]):Option[R] = dbObject.map(x=>converter(x))
  
  
  // Required for MongoCollectionBase extending:
  def underlying = mongoCollection.underlying
  override type T = DBObject
  override def _newInstance(collection: DBCollection): MongoCollectionBase = new TypedMongoCollection[R](mongoCollection,converter)
  override def _newCursor(cursor: DBCursor): CursorType = new TypedMongoCursor[R](new MongoCursor(cursor))(converter)
  override type CursorType = TypedMongoCursor[R]
}

object TypedMongoCollection{
  @inline implicit def typedMongoCollectionAsMongoCollection[R](t: TypedMongoCollection[R]):MongoCollection = t.mongoCollection
  @inline implicit def typedMongoCollectionAsDBCollection[R](t: TypedMongoCollection[R]):DBCollection = t.mongoCollection.underlying
}
