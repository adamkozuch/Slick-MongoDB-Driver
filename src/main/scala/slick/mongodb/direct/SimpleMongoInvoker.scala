package slick.mongodb.direct

import com.mongodb.DBObject
import com.mongodb.util.JSON

import slick.mongodb.MongoInvoker

class SimpleMongoInvoker[T](override val mongoCollection: TypedMongoCollection[T],override val query: Option[DBObject]) extends MongoInvoker[T]
object SimpleMongoInvoker{
  def apply[P,R](collectionName: String, queryString: Option[String],queryParameters: Option[P])
                (implicit session: MongoBackend#Session, converter: GetResult[R]): SimpleMongoInvoker[R] = {
    val typedMongoCollection = new TypedMongoCollection[R](collectionName)(session,converter)
    new SimpleMongoInvoker[R](typedMongoCollection,parsedQuery(queryString,queryParameters))
  }

  import slick.mongodb.direct.MongoInterpolation._
  private def interpolatedQuery[P](queryString:Option[String], queryParameters: Option[P]): Option[String] = queryParameters match{
    case Some(parameters) => queryString.map(interpolate[P](_,parameters))
    case None => queryString
  }

  private def parsedQuery[P](queryString: Option[String],queryParameters: Option[P]) =
    interpolatedQuery[P](queryString,queryParameters).map(JSON.parse(_).asInstanceOf[DBObject])

}
