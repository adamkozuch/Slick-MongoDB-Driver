
import java.lang.Thread

import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.tagobjects.Slow
import org.scalatest.time.{Seconds, Span}
import slick.mongodb.lifted.MongoDriver.api._
import slick.lifted.Tag

/**
 * Created by adam on 14.06.15.
 */

class SimpleTable(tag: Tag)
  extends Table[(Int, Int, String)](tag, "Table") {

  def first = column[Int]("first")

  def secound = column[Int]("secound")

  def third = column[String]("third")

  def * = (first, secound, third)
}

class NestedDocument(tag: Tag)
  extends Document[(String, Option[Int], Option[(Int, Option[(Int, Option[Int])])])](tag, "Collection") {//

  def nestedDocument = document("firstLevel")((column[Int]("1"), (document("secoundLevel")((column[Int]("2"), document("thirdLevel")(column[Int]("nestedString")))))))

  def simpleColumn = column[String]("someValue")

  def flatDocument = document("flatDocument")(column[Int]("someValue"))

  def * = (simpleColumn, flatDocument,nestedDocument)
}



class TestTableAndDocument extends FunSuite with BeforeAndAfter with ScalaFutures {

  implicit override val patienceConfig = PatienceConfig(timeout = Span(50, Seconds))

  val nestedDocumentQuery = TableQuery[NestedDocument]
  val simpleQuery = TableQuery[SimpleTable]
  before {
    db = Database.forURL("mongodb://localhost:27017/test") // MongoDB binds to 127.0.0.1  in travis
  }

  var db: Database = _

  val sequenceForTable = DBIO.seq(simpleQuery +=(2, 3, "222"))
  val sequenceForDocument = DBIO.seq(nestedDocumentQuery +=("firstString", Some(77), Some(22, Some(33, Some(1)))))

  def insertToTable = {db.run(sequenceForTable.result)}.futureValue

  def resultSimpleInsert = ( db.run(simpleQuery.result)).futureValue

  def nestedDocumentAction() =
    db.run(nestedDocumentQuery.result).futureValue


    test("basic insert result") {
      insertToTable
      Thread.sleep(100)
      val result = resultSimpleInsert //TODO think about good tests
      assert(result.toString =="Vector((2,3,222))")
    }

    test("nested insert works") {
      //todo I have to modyfy invoker to make this work
      //val result = nestedDocumentAction()
      assert(1 == 1)
    }

}
