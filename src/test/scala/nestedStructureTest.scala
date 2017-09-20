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
case class first(x: Int, secondLevel: second, y: IndexedSeq[String]) extends doc.NewTerm
case class second(c: Int, thirdLevel: third1, thirdLevel2:third2) extends doc.NewTerm
case class third1(c: Int, fourthLevel: fourth) extends doc.NewTerm
case class third2(c: Double, s: String) extends doc.NewTerm
case class fourth(c: Int, d: IndexedSeq[Int]) extends doc.NewTerm



class firstLevelDocument(tags:Tag) extends Document[first](tags,"firstLevelDocument") {

  def x1 = field[Int]("primitiveFieldFirstLevel")
  def secondDoc = doc[secoundLevelDocument](tags)
  def arrOfString = array(field[String]("y"))
  def * = (x1, secondDoc, arrOfString) <> (first.tupled, first.unapply)


}

class secoundLevelDocument(tag:Tag) extends SubDocument[second](tag,"secoundLevelDocument") {
  type previousDocument = firstLevelDocument
  def x2 = field[Int]("primitiveFieldSecundLevel")
  def thirdDoc1 = doc[thirdLevelDocument1](tag)
  def thirdDoc2 = doc[thirdLevelDocument2](tag)
  def * = (x2, thirdDoc1, thirdDoc2) <> (second.tupled, second.unapply)
}

class thirdLevelDocument1(tag:Tag) extends SubDocument[third1](tag,"thirdLevelDocument1") {
  type previousDocument = secoundLevelDocument
  def x3 = field[Int]("primitiveFieldThirdLevel")
  def dyn = field[Int]("DynamicprimitiveFieldThirdLevel")
  def docDyn = doc[noProjection](tag)
  def arrayOfFurthDoc = doc[fourthLevelDocument](tag)
  def * = (x3, arrayOfFurthDoc) <> (third1.tupled, third1.unapply)
}

class thirdLevelDocument2(tag:Tag) extends SubDocument[third2](tag,"thirdLevelDocument2") {
  type previousDocument = secoundLevelDocument
  def x3 = field[Double]("primitiveFieldThirdLevel11")
  def x4 = field[String]("primitiveFieldThirdLevel22")
  def * = (x3, x4) <> (third2.tupled, third2.unapply)
}


class fourthLevelDocument(tag:Tag) extends SubDocument[fourth](tag,"fourthLevelDocument") {
  def x4 = field[Int]("firstPrimitiveFieldFourthLevel")
  def arrOfInt = array(field[Int]("PrimitiveFieldFourthLevelForArray"))
  def * = (x4, arrOfInt) <> (fourth.tupled, fourth.unapply)
}


class additional1(tag:Tag) extends SubDocument[fourth](tag,"add1") {
  def x4 = field[Int]("firstPrimitiveFieldFourthLevel")
  def arrOfInt = array(field[Int]("PrimitiveFieldFourthLevelForArray"))
  def * = (x4, arrOfInt) <> (fourth.tupled, fourth.unapply)
}


class additional2(tag:Tag) extends SubDocument[fourth](tag,"add2") {
  def x4 = field[Int]("firstPrimitiveFieldFourthLevel")
  def arrOfInt = array(field[Int]("PrimitiveFieldFourthLevelForArray"))
  def * = (x4, arrOfInt) <> (fourth.tupled, fourth.unapply)
}

case class noProj(c: Int, s: Int)


class noProjection(tag:Tag) extends SubDocument[noProj](tag,"noProjectionDocument") {

  def x4 = field[Int]("noProjectionField1")
  def next =doc[fourthLevelDocument](tag)
  def arrOfInt =field[Int]("noProjectionField2")

  def * = (x4, arrOfInt) <> (noProj.tupled, noProj.unapply)
}



class nestedStructureTest extends FunSuite with BeforeAndAfter with ScalaFutures {

  implicit override val patienceConfig = PatienceConfig(timeout = Span(50, Seconds))

  val documentQuery = TableQuery[firstLevelDocument]


  before {
    db = Database.forURL("mongodb://localhost:27017/test") // MongoDB binds to 127.0.0.1  in travis
  }

  var db: Database = _


  test("third level document select")
  {
  lazy val result =  (db.run(documentQuery.map(x => x.secondDoc.thirdDoc1).result)).futureValue
    //result
  }


  test("two level field select ")
  {
    lazy val result = ( db.run(documentQuery.map(x => x.secondDoc.x2).result)).futureValue
    println("What kind of result " + result)
  }

  test("second level document select")
  {
    lazy  val result = ( db.run(documentQuery.map(x=>x.secondDoc.thirdDoc1).result)).futureValue
    //println("This is the result" + result.toString())
  }


  test("filter by nested field")
  {
    lazy val result = ( db.run(documentQuery.map(x => x.secondDoc.thirdDoc2).result)).futureValue
    //println("This is result2" + result)
  }


  test("nested insert")
  {
    val simpleInsert = DBIO.seq(documentQuery ++=
      List(
        first(5,second(1,third1(33,fourth(1,IndexedSeq(1,2,3))), third2(10.0,"ala ma kota") ),IndexedSeq("a", "b", "c")  ),
        first(9,second(77,third1(33,fourth(1,IndexedSeq(1,2,3))), third2(10.0,"ala ma kota") ),IndexedSeq("a", "b", "c")  )
      ))
    lazy val result =  (db.run(simpleInsert)).futureValue
    result
  }

}
