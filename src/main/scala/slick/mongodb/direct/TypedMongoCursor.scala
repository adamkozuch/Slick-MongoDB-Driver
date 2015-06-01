package slick.mongodb.direct

import com.mongodb.casbah.Imports._
import com.mongodb.casbah.{CursorExplanation, MongoCursor}

import slick.util.CloseableIterator

/**
 * Wraps Casbah's MongoCursor, so that values returned from TypedMongoCursor
 * are converted to user-specified Scala types, not MongoDBObject
 * @param underlying wrapped MonogCursor
 * @param converter converterts mongo DBObject to specified Scala type
 * @tparam R type to convert DBObject to
 */
class TypedMongoCursor[+R](val underlying: MongoCursor)(implicit val converter: GetResult[R]) extends CloseableIterator[R] {

  /**
   * next
   *
   * Iterator increment.
   *
   * @return The next element in the cursor
   */
  def next() = converter(underlying.next)

  /**
   * hasNext
   *
   * Is there another element in the cursor?
   *
   * @return (Boolean Next)
   */
  def hasNext = underlying.hasNext

  /**
   * sort
   *
   * Sort this cursor's elements
   *
   * @param  orderBy (A) The fields on which to sort
   * @tparam A  A view of DBObject to sort by
   * @return A cursor pointing to the first element of the sorted results
   */
  def sort[A <% DBObject](orderBy: A): TypedMongoCursor[R] = _newInstance(underlying.sort(orderBy))

  /**
   * count
   *
   * The DBCursor's count of elements in the query, passed through
   * from the Java object.  Note that Scala Iterator[_] base trait
   * has a count method which tests predicates and you should
   * be careful about using that.
   *
   * <b>NOTE:</b> count() ignores any skip/limit settings on a cursor;
   * it is the count of the ENTIRE query results.
   * If you want to get a count post-skip/limit
   * you must use size()
   *
   * @see size()
   *
   * @return Int indicating the number of elements returned by the query
   * @throws MongoException
   */
  def count = underlying.count

  /**
   * Manipulate Query Options
   *
   * Adds an option - see Bytes.QUERYOPTION_* for list
   * TODO - Create Scala version of Bytes.QUERYOPTION_*
   *
   * @see com.mongodb.Mongo
   * @see com.mongodb.Bytes
   */
  def option_=(option: Int): Unit = underlying.option_=(option)

  /**
   * Manipulate Query Options
   *
   * Gets current option settings - see Bytes.QUERYOPTION_* for list
   *
   * @see com.mongodb.Mongo
   * @see com.mongodb.Bytes
   */
  def option = underlying.option

  /**
   * Manipulate Query Options
   *
   * Resets options to default.
   *
   * @see com.mongodb.Mongo
   * @see com.mongodb.Bytes
   */
  def resetOptions() = underlying.resetOptions()

  /**
   * Manipulate Query Options
   *
   * Gets current option settings - see Bytes.QUERYOPTION_* for list
   *
   * @see com.mongodb.Mongo
   * @see com.mongodb.Bytes
   */
  def options = underlying.options

  /**
   * Manipulate Query Options
   *
   * Sets current option settings - see Bytes.QUERYOPTION_* for list
   *
   * @see com.mongodb.Mongo
   * @see com.mongodb.Bytes
   */
  def options_=(opts: Int): Unit = underlying.options_=(opts)

  /**
   * hint
   *
   * Provide the Database a hint of which indexed fields of a collection to use
   * to improve performance.
   *
   * @param  indexKeys (A) A DBObject of the index names as keys
   * @tparam A A view of DBObject to use for the indexKeys
   * @return the same DBCursor, useful for chaining operations
   */
  def hint[A <% DBObject](indexKeys: A): TypedMongoCursor[R] = _newInstance(underlying.hint(indexKeys))

  /**
   * hint
   *
   * Provide the Database a hint of an indexed field of a collection to use
   * to improve performance.
   *
   * @param  indexName (String) The name of an index
   * @return the same DBCursor, useful for chaining operations
   */
  def hint(indexName: String): TypedMongoCursor[R] = _newInstance(underlying.hint(indexName))

  /**
   * snapshot
   *
   * Use snapshot mode for the query.
   * Snapshot mode assures no duplicates are returned, or objects missed,
   * which were present at both the start and end of the query's
   * execution (if an object is new during the query, or deleted during the query,
   * it may or may not be returned, even with snapshot mode).
   *
   * <b>NOTE:</b> Short query responses (&lt; 1MB) are always effectively snapshotted.
   * <b>NOTE:</b> Currently, snapshot mode may not be used with sorting or explicit hints.
   *
   * @return the same DBCursor, useful for chaining operations
   */
  def snapshot(): TypedMongoCursor[R] = _newInstance(underlying.snapshot())

  /**
   * explain
   *
   * Returns an object containing basic information about the execution
   * of the query that created this cursor.
   * This creates an instance of CursorExplanation which is a custom
   * dbObject with the key/value pairs:
   *     - cursor = Cursor Type
   *     - nscanned = Number of items examined by Mongo for this query
   *     - nscannedObjects = Number of objects examined by Mongo
   *     - n = the number of records which Mongo returned
   *     - millis = how long it took Mongo to execute the query
   *     - nYields = number of times this query yielded the read lock to let writes in
   *     - indexBounds = the index boundaries used.
   *
   * CursorExplanation provides utility methods to access these fields.
   *
   * @see http://dochub.mongodb.org/core/explain
   * @return an instance of CursorExplanation
   */
  def explain = new CursorExplanation(underlying.explain)

  /**
   * limit
   *
   * Limits the number of elements returned.
   *
   * <b>NOTE:</b> Specifying a <em>negative number</em> instructs
   * the server to retrun that number of items and close the cursor.
   * It will only return what can fit into a <em>single 4MB response</em>
   *
   * @param  n (Int)  The number of elements to return
   * @return A cursor pointing to the first element of the limited results
   *
   * @see http://dochub.mongodb.org/core/limit
   */
  def limit(n: Int): TypedMongoCursor[R] = _newInstance(underlying.limit(n))

  /**
   * skip
   *
   * Discards a given number of elements at the beginning of the cursor.
   *
   * @param  n (Int)  The number of elements to skip
   * @return A cursor pointing to the first element of the results
   *
   * @see http://dochub.mongodb.org/core/skip
   */
  def skip(n: Int): TypedMongoCursor[R] = _newInstance(underlying.skip(n))

  /**
   * cursorId
   *
   * @return A long representing the cursorID on the server; 0 = no cursor
   *
   */
  def cursorId = underlying.cursorId

  /**
   * close
   *
   * Kill the current cursor on the server
   */
  def close() = underlying.close()

  def numGetMores = underlying.numGetMores

  /**
   * numSeen
   *
   * Returns the number of objects through which this cursor has iterated,
   * as tracked by the java driver.
   *
   * @return The number of objects seen
   */
  def numSeen = underlying.numSeen

  def sizes = underlying.sizes

  /**
   * batchSize
   *
   * Limits the number of elements returned in one batch.
   *
   * @param  n (Int) The number of elements to return in a batch
   * @return the same DBCursor, useful for chaining operations
   */
  def batchSize(n: Int) = {
    underlying.batchSize(n)
    this
  }

  def keysWanted = underlying.underlying.getKeysWanted

  def query = underlying.query

  /**
   * "Special" Operators for cursors
   *
   * adds a special operator like \$maxScan or \$returnKey
   * @see http://www.mongodb.org/display/DOCS/Advanced+Queries#AdvancedQueries-Specialoperators
   * @return the same DBCursor, useful for chaining operations
   */
  def addSpecial(name: String, o: Any): TypedMongoCursor[R] = _newInstance(underlying.addSpecial(name,o))

  /**
   * \$returnKey
   *
   * Sets a special operator of \$returnKey
   * If true, returns ONLY the index key.
   * Defaults to true if you just call \$returnKey
   *
   * @param  bool (Boolean = true)
   * @return the same DBCursor, useful for chaining operations
   */
  def $returnKey(bool: Boolean = true): TypedMongoCursor[R] = addSpecial("$returnKey", bool)

  /**
   * \$maxScan
   *
   * Sets a special operator of \$maxScan
   * Which defines the max number of items to scan.
   *
   * @param  max (A)
   * @tparam A : Numeric
   * @return the same DBCursor, useful for chaining operations
   */
  def $maxScan[A: Numeric](max: A): TypedMongoCursor[R] = addSpecial("$maxScan", max)

  /**
   * \$query
   *
   * Sets a special operator of \$query
   * Which defines the query for this cursor.
   *
   * This is the same as running find() on a Collection with the query.
   *
   * @param  q (DBObject)
   * @return the same DBCursor, useful for chaining operations
   */
  def $query[A <% DBObject](q: A): TypedMongoCursor[R] = addSpecial("$query", q)

  /**
   * \$orderby
   *
   * Sets a special operator of \$orderby
   * which defines the sort spec for this cursor.
   *
   * This is the same as calling sort on the cursor.
   *
   * @param  obj (DBObject)
   * @return the same DBCursor, useful for chaining operations
   */
  def $orderby[A <% DBObject](obj: A): TypedMongoCursor[R] = addSpecial("$orderby", obj)

  /**
   * \$explain
   *
   * Sets a special operator of \$explain
   * which, if true, explains the query instead of returning results.
   *
   * This is the same as calling the explain() method on the cursor.
   *
   * @param  bool (Boolean = true)
   * @return the same DBCursor, useful for chaining operations
   */
  def $explain(bool: Boolean = true): MongoCursor = underlying.$explain(bool)

  /**
   * \$snapshot
   *
   * Sets a special operator of \$snapshot
   * which, if True, sets snapshot mode on the query.
   *
   * This is the same as calling the snapshot() method on the cursor.
   *
   * @param  bool (Boolean = true)
   * @return the same DBCursor, useful for chaining operations
   */
  def $snapshot(bool: Boolean = true): TypedMongoCursor[R] = _newInstance(underlying.$snapshot(bool))

  /**
   * \$min
   *
   * Sets minimum index bounds - commonly paired with \$max
   *
   * @param  obj (DBObject)
   * @see http://www.mongodb.org/display/DOCS/min+and+max+Query+Specifiers
   *
   * @return the same DBCursor, useful for chaining operations
   */
  def $min[A <% DBObject](obj: A): TypedMongoCursor[R] = _newInstance(underlying.$min(obj))

  /**
   * \$max
   *
   * Sets maximum index bounds - commonly paired with \$max
   *
   * @param  obj (DBObject)
   * @see http://www.mongodb.org/display/DOCS/max+and+max+Query+Specifiers
   *
   * @return the same DBCursor, useful for chaining operations
   */
  def $max[A <% DBObject](obj: A): TypedMongoCursor[R] = _newInstance(underlying.$max(obj))

  /**
   * \$showDiskLoc
   *
   * Sets a special operator \$showDiskLoc which, if true,
   * shows the disk location of results.
   *
   * @param  bool (Boolean = true)
   * @return the same DBCursor, useful for chaining operations
   */
  def $showDiskLoc(bool: Boolean = true): TypedMongoCursor[R] = _newInstance(underlying.$showDiskLoc(bool))

  /**
   * \$hint
   *
   * Sets a special operator \$hint which
   * forces the query to use a given index.
   *
   * This is the same as calling hint() on the cursor.
   *
   * @param obj (DBObject)
   * @return the same DBCursor, useful for chaining operations
   */
  def $hint[A <% DBObject](obj: A): TypedMongoCursor[R] = _newInstance(underlying.$hint(obj))

  /**
   * copy
   *
   * Creates a new copy of an existing database cursor.
   * The new cursor is an iterator even if the original
   * was an array.
   *
   * @return The new cursor
   */
  def copy(): TypedMongoCursor[R] = _newInstance(underlying.copy()) 


  /**
   * size
   *
   * The DBCursor's count of elements in the query,
   * AFTER the application of skip/limit, passed through
   * from the Java object.
   *
   * <b>NOTE:</b> size() takes into account any skip/limit settings on a cursor;
   * it is the size of just the window.
   * If you want to get a count of the entire query ignoring skip/limit
   * you must use count()
   *
   * @see count()
   *
   * @return Int indicating the number of elements returned by the query after skip/limit
   * @throws MongoException
   */
  override def size = underlying.size

  /**
   * _newInstance
   *
   * Utility method which concrete subclasses
   * are expected to implement for creating a new
   * instance of THIS concrete implementation from a
   * Java cursor.  Good with cursor calls that return a new cursor.
   *
   * @param  cursor (DBCursor)
   * @return (TypedMongoCursor[R])
   */
  def _newInstance(cursor: MongoCursor) = new TypedMongoCursor(cursor)
}
