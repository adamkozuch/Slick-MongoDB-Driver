
import java.lang.Thread

import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.tagobjects.Slow
import org.scalatest.time.{Seconds, Span}
import slick.mongodb.lifted.MongoDriver.api._
import slick.lifted.{ProvenShape, Tag}
import slick.mongodb.types.doc

/**
 * Created by adam on 14.06.15.
 */

class SimpleDocument(tag: Tag)
  extends Document[(Int, Int, String)](tag, "SimpleCollection") {

  def first = field[Int]("first")

  def secound = field[Int]("secound")

  def third = field[String]("third")

  def * = (first, secound, third)
}

case class TS(s:Int,g:Int,c:String)
class SimpleDcumentTypeMapping(tag: Tag)
  extends Document[TS](tag, "SimpleCollectionTypeMapping") {

  def first = field[Int]("first")

  def secound = field[Int]("secound")

  def third = field[String]("third")

  def * = (first, secound, third)<>(TS.tupled, TS.unapply)
}


class SimpleStructureTest extends FunSuite with BeforeAndAfter with ScalaFutures {

  implicit override val patienceConfig = PatienceConfig(timeout = Span(50, Seconds))

  val simpleDocumentQuery = TableQuery[SimpleDocument]
  val simpleDocumentQueryTypeMapping = TableQuery[SimpleDcumentTypeMapping]


  before {
    db = Database.forURL("mongodb://localhost:27017/test") // MongoDB binds to 127.0.0.1  in travis
  }

  var db: Database = _

  val simpleInsertTM = DBIO.seq(simpleDocumentQueryTypeMapping +=(TS(1,2,"Some text")))
  val simpleInsert = DBIO.seq(simpleDocumentQuery +=(1,2,"Some text"))

  def simpleInsertAction = {db.run(simpleInsert)}.futureValue

  def simpleInsertActionTM = ( db.run(simpleInsertTM)).futureValue


  
  test("basic insert ") {
      simpleInsertAction
      Thread.sleep(100)
    val result =  {db.run(simpleDocumentQuery.result)}.futureValue
    Thread.sleep(100)
    //assert(result.toString ==="Vector(2,Some text))")
    }
  test("basic map ")
  {
    simpleInsertAction
    Thread.sleep(100)
    val result =   ( db.run(simpleDocumentQuery.map(x =>x.secound).result)).futureValue
    Thread.sleep(100)
 //   assert(result.toString ==="Vector(2)")
  }

  test("basic insert type mapping") {
      simpleInsertActionTM
      Thread.sleep(100)
      val result =  ( db.run(simpleDocumentQueryTypeMapping.result)).futureValue
  //    assert(result.toString ==="Vector((1,2,Some text))")
    }

  test("basic map type mapping ")
  {
    simpleInsertActionTM
    Thread.sleep(100)
    val result =   ( db.run(simpleDocumentQueryTypeMapping.map(x =>x.secound).result)).futureValue
    //assert(result.toString =="Vector(2)")
  }


}
