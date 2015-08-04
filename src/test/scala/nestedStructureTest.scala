import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.tagobjects.Slow
import org.scalatest.time.{Seconds, Span}
import slick.mongodb.lifted.MongoDriver.api._
import slick.lifted.{ProvenShape, Tag}
import slick.mongodb.types.doc

/**
 * Created by adam on 10.07.15.
 */

case class first(x: Int, secondLevel: second, y: IndexedSeq[String])
case class second(c: Int, thirdLevel: third)
case class third(c: Int, fourthLevel: IndexedSeq[fourth])
case class fourth(c: Int, d: IndexedSeq[Int])


class firstLevelDocument(tags:Tag) extends Document[first](tags,"firstLevelDocument") {

  def x1 = field[Int]("primitiveFieldFirstLevel")
  def secondDoc = doc[secoundLevelDocument](tags,tableName)
  def arrOfString = array(field[String]("y"))
  def * = (x1, secondDoc, arrOfString) <> (first.tupled, first.unapply)


}

class secoundLevelDocument(tag:Tag) extends SubDocument[second](tag,"secoundLevelDocument") {
  type previousDocument = firstLevelDocument
  def x2 = field[Int]("primitiveFieldSecundLevel")
  def thirdDoc = doc[thirdLevelDocument](tag, tableName)
  def * = (x2, thirdDoc) <> (second.tupled, second.unapply)
}

class thirdLevelDocument(tag:Tag) extends SubDocument[third](tag,"thirdLevelDocument") {
  type previousDocument = secoundLevelDocument
  def x3 = field[Int]("primitiveFieldThirdLevel")
  def arrayOfFurthDoc = array(doc[fourthLevelDocument](tag,tableName))
  def * = (x3, arrayOfFurthDoc) <> (third.tupled, third.unapply)
}


class fourthLevelDocument(tag:Tag) extends SubDocument[fourth](tag,"fourthLevelDocument") {
  type previousDocument = thirdLevelDocument

  def x4 = field[Int]("firstPrimitiveFieldFourthLevel")
  def arrOfInt = array(field[Int]("PrimitiveFieldFourthLevelForArray"))
  def * = (x4, arrOfInt) <> (fourth.tupled, fourth.unapply)
}




class nestedStructureTest extends FunSuite with BeforeAndAfter with ScalaFutures {

  implicit override val patienceConfig = PatienceConfig(timeout = Span(50, Seconds))

  val documentQuery = TableQuery[firstLevelDocument]

  before {
    db = Database.forURL("mongodb://localhost:27017/test") // MongoDB binds to 127.0.0.1  in travis
  }

  var db: Database = _

  /**
   * slick.ast.Comprehension cannot be cast to slick.ast.Select.
   * LiftedMongoInvoker.scala:20
   */
  test("third level document select")
  {
  lazy val result =  (db.run(documentQuery.map(x => x.secondDoc.thirdDoc).result)).futureValue
   // result
  }

  /**
   * java.util.NoSuchElementException, with message: None.get
   */
  test("two level field select ")
  {
   lazy val result = ( db.run(documentQuery.map(x => x.secondDoc.x2).result)).futureValue
    result
  }

  /**
   * slick.ast.Comprehension cannot be cast to slick.ast.Select
   */
  test("secoud level document select")
  {
  lazy  val result = ( db.run(documentQuery.map(x => x.secondDoc.thirdDoc.x3).result)).futureValue
    result
  }

  /**
   * scala.MatchError, with message: Select c (of class slick.ast.Select)
   * singleArgumentFunctionParameters(LiftedMongoInvoker.scala:181)
   */
  test("filter by nested field")
  {
  lazy val result = ( db.run(documentQuery.sortBy(_.x1).filter(x => x.secondDoc.thirdDoc.x3>7).map(x => x.secondDoc.thirdDoc).result)).futureValue
    result
  }

  /**
   * scala.MatchError, with message: TableExpansion (of class slick.ast.TableExpansion).
   * in liftedInvoker
   */
  test("nested insert")
  {
    val simpleInsert = DBIO.seq(documentQuery +=(first(1,second(4,third(5,IndexedSeq(fourth(50,IndexedSeq(2,3,4,5)),fourth(50,IndexedSeq(2,3,4,5))))), IndexedSeq("jdjd","jdjjd"))))
   lazy val result =  (db.run(simpleInsert)).futureValue
    //result
  }

}
