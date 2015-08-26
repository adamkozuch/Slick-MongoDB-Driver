package slick.mongodb.lifted

import com.mongodb.casbah.Imports._
import com.mongodb.casbah.commons.MongoDBObject
import slick.ast._
import slick.mongodb.direct.GetResult

/**
 * Created by adam on 26.08.15.
 */
class Converter[R](t:Type) {

  val converter: GetResult[R] ={ GetResult[R](r => {
    //todo
    r.asInstanceOf[R]
  })}
}
