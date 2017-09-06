package slick.mongodb

import com.mongodb.DBObject

import slick.mongodb.direct.{TypedMongoCursor, MongoBackend, TypedMongoCollection}



import slick.GenericInvoker


trait MongoInvoker[T] extends GenericInvoker[T] {
  def mongoCollection: TypedMongoCollection[T]
  def query: Option[DBObject]

  override type Session = MongoBackend#Session

  /** Execute the statement and return the first row of the result set wrapped
    * in Some, or None if the result set is empty. */
  override def firstOption(implicit session: Session): Option[T] = query match{
    case Some(q) => mongoCollection.findOneTyped(q)
    case None => mongoCollection.findOneTyped()
  }

  /** Execute the statement and return a CloseableIterator of the converted
    * results. The iterator must either be fully read or closed explicitly.
    * @param maxRows Maximum number of rows to read from the result (0 for unlimited). */
  override def iteratorTo(maxRows: Int)(implicit session: Session): TypedMongoCursor[T] = {
    iterator.limit(maxRows)
  }

  /** Execute the statement and return a CloseableIterator of the converted
    * results. The iterator must either be fully read or closed explicitly. */
  override def iterator(implicit session: Session): TypedMongoCursor[T] = query match{
    case Some(q) => mongoCollection.find(q)
    case None => mongoCollection.find()
  }
}
