package slick.mongodb.direct

import com.mongodb.DBObject
import com.mongodb.casbah.commons.MongoDBObject

import scala.language.implicitConversions
import slick.mongodb.direct.MongoInterpolation._


// TODO: finish documentation here
/**
 *
 * MongoQuery points to single collection in a MongoDB
 *
 * @param collectionName Name of the collection in the MongoDB to fetch data from
 * @param queryString Represents the query that is used to fetch data
 * @tparam R Result type of the query
 */
// TODO: see if we can refactor to simplify usage of converter here - probably we can use Salat to perform conversions automatically
// TODO: check if we can make R covariant
class MongoQuery[-P,R](val collectionName:String,val queryString: Option[String]) extends ((Option[P],MongoBackend#Session,GetResult[R]) => SimpleMongoInvoker[R]){

  override def apply(queryParameters: Option[P], session: MongoBackend#Session, converter: GetResult[R]): SimpleMongoInvoker[R] =
    SimpleMongoInvoker[P,R](collectionName,queryString,queryParameters)(session,converter)

  // Hack with swapping session and converter parameters is required to make them implicit
  def apply(queryParameters: P)(implicit converter: GetResult[R], session: MongoBackend#Session): SimpleMongoInvoker[R] =
    apply(Some(queryParameters),session,converter)
}

// TODO: think how collection name may be received implicitly from result type - some macro required
// Example: instead of
// Q.query[Employee]("employee","{name:John}") foreach{...}
// I want to see
// Q.query[Employee]("{name:John}") foreach{...}
// however, this conflicts with
// Q.query[Employee]("employee") foreach{...}
// which is also useful when you don't want to apply any filters
object MongoQuery{

  //TODO: check if static apply method conflicts with apply method of the MongoQuery instance
  def apply[R](collection:String) = query[R](collection)

  def apply[P,R](collection:String,filter:String) = query[P,R](collection,filter)

  def query[R](collection: String) = new MongoQuery[Unit,R](collection, None)

  def query[P,R](collection: String, filter: String) = new MongoQuery[P,R](collection, Some(filter))

  @inline implicit def mongoQueryAsInvoker[R](s: MongoQuery[Unit, R])(implicit session: MongoBackend#Session, converter: GetResult[R]): SimpleMongoInvoker[R] =
    s.apply(None,session,converter)

  @inline implicit def mongoQueryAsTypedMongoCollection[R](s: MongoQuery[Unit, R])(implicit session: MongoBackend#Session, converter: GetResult[R]): TypedMongoCollection[R] =
    new TypedMongoCollection[R](s.collectionName)(session,converter)

  @inline implicit def mongoDBObjectAsR[R](mongoDBObject: MongoDBObject)(implicit converter: GetResult[R]):R = converter(mongoDBObject)

  @inline implicit def DBObjectAsR[R](dbObject: DBObject)(implicit converter: GetResult[R]):R = converter(new MongoDBObject(dbObject))

  implicit class MongoInterpolation(val s: StringContext) extends AnyVal{
    def mq(parameters: Any*) = new MongoInterpolationResult(s,parameters: _*)
  }

  class MongoInterpolationResult(val s: StringContext,val parameters: Any*){
    def in[R](collectionName: String): MongoQuery[Unit,R] =
      MongoQuery.query[Unit, R](collectionName,s.s(new ParametersKeeper[Seq[Any]](parameters).iterator.toSeq: _*))
  }

}