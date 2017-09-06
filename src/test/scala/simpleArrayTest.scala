import com.mongodb.casbah.commons.ValidBSONType.BasicDBList
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Seconds, Span}
import slick.ast.{Select, Pure, ScalaBaseType}
import slick.lifted.{Tag, QueryBase, RepShape}
import slick.mongodb.lifted.MongoDriver.api._
import slick.mongodb.types.{doc}

/**
* Created by adam on 03.07.15.
*/


class DcumentWithArrays(tag: Tag)
  extends Document[(IndexedSeq[Int], IndexedSeq[IndexedSeq[Int]])](tag, "CollectionOfArrays") {

  def oneLevelPrimitiveArray: Query[Rep[Int], Int, IndexedSeq] = array(field[Int]("ss"))

  def twoLevelsPrimitiveArray = array(array(field[Int]("someField")))

  def * = (oneLevelPrimitiveArray, twoLevelsPrimitiveArray)
}

case class withPrimitiveArrays(first: IndexedSeq[Int], secound: IndexedSeq[IndexedSeq[String]])

class DcumentWIthArraysMap(tag: Tag)
  extends Document[withPrimitiveArrays](tag, "CollectionOfArraysTypeMapping") {

  def oneLevelPrimitiveArray: Query[Rep[Int], Int, IndexedSeq] = array(field[Int]("ss"))

  def twoLevelsPrimitiveArray = array(array(field[String]("someField")))

  def * = (oneLevelPrimitiveArray, twoLevelsPrimitiveArray) <>(withPrimitiveArrays.tupled, withPrimitiveArrays.unapply)
}


class simpleArrayTest extends FunSuite with BeforeAndAfter with ScalaFutures {
  implicit override val patienceConfig = PatienceConfig(timeout = Span(50, Seconds))


  val arrayQuery = TableQuery[DcumentWithArrays]
  val arrayQueryMap = TableQuery[DcumentWIthArraysMap]


  before {
    db = Database.forURL("mongodb://localhost:27017/test") // MongoDB binds to 127.0.0.1  in travis
  }

  var db: Database = _


  test("array insert result") {
    val insert = DBIO.seq(arrayQuery +=(IndexedSeq(1, 2, 3, 4), IndexedSeq(IndexedSeq(3, 4, 5), IndexedSeq(7, 8, 9))))

//    {
//      db.run(insert)
//    }.futureValue
    Thread.sleep(100)
  lazy  val result1 = {
      db.run(arrayQuery.map(x => x.twoLevelsPrimitiveArray).result)
    }.futureValue
  lazy  val result2 = {
      db.run(arrayQuery.map(x => x.oneLevelPrimitiveArray).result)
    }.futureValue
  lazy  val result3 = {
      db.run(arrayQuery.result)
    }.futureValue

    assert(result1.toString == "Vector(([ [ 3 , 4 , 5] , [ 7 , 8 , 9]]))")
//    assert(result2.toString == "Vector(([ 1 , 2 , 3 , 4]))")
//    assert(result3.toString == "Vector(([ 1 , 2 , 3 , 4],[ [ 3 , 4 , 5] , [ 7 , 8 , 9]]))")

  }

  test("type mapping darray insert result") {
    val insert = DBIO.seq(arrayQueryMap += (withPrimitiveArrays(IndexedSeq(1, 2, 3, 4), IndexedSeq(IndexedSeq("a", "b", "c"), IndexedSeq("c", "d", "e")))))

    lazy val result  =   {
      db.run(insert)
    }.futureValue
    Thread.sleep(100)
    lazy  val result1 = {
      db.run(arrayQueryMap.map(x => x.twoLevelsPrimitiveArray).result)
    }.futureValue
    lazy   val result2 = {
      db.run(arrayQueryMap.map(x => x.oneLevelPrimitiveArray).result)
    }.futureValue
    lazy  val result3 = {
      db.run(arrayQueryMap.result)
    }.futureValue

    //    assert(result1.toString == "Vector(([ [ 3 , 4 , 5] , [ 7 , 8 , 9]]))")
    //    assert(result2.toString == "Vector(([ 1 , 2 , 3 , 4]))")
    //    assert(result3.toString == "Vector(([ 1 , 2 , 3 , 4],[ [ 3 , 4 , 5] , [ 7 , 8 , 9]]))")

  }

  /**
   * Expected a collection type, found Int
   * slick.ast.TypeUtil$.asCollectionType$extension(Type.scala:278)
   */
  test("queryPrimitive array") {
     lazy val result =    (db.run(arrayQuery.map(x => x.oneLevelPrimitiveArray.filter(x => x > 5)).result)).futureValue
  //  result
  }
}
