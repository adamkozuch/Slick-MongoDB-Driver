import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Seconds, Span}
import slick.mongodb.lifted.MongoDriver.api._
import slick.lifted.{ProvenShape, Tag}
import slick.mongodb.types.doc
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Created by adam on 10.07.15.
 */
case class first(x: Int, secondLevel: second) extends doc.NewTerm
case class second(c: Int, thirdLevel1:third2) extends doc.NewTerm
case class third1(c: Int, g:Int) extends doc.NewTerm
case class third2(c: Double, s: String) extends doc.NewTerm
case class fourth(c: Int, d: Int) extends doc.NewTerm

class firstLevelDocument(tags:Tag) extends Document[first](tags,"firstLevelDocument") {

  def x1 = field[Int]("primitiveFieldFirstLevel")
  def secondDoc = doc[secoundLevelDocument](tags)
//  def arrOfString = array(field[String]("y"))
  def * = (x1, secondDoc) <> (first.tupled, first.unapply)  // tutaj chodzi o to że dokument jest nested
}

class secoundLevelDocument(tag:Tag) extends SubDocument[second](tag,"secoundLevelDocument") {
  type previousDocument = firstLevelDocument
  def x2 = field[Int]("primitiveFieldSecundLevel")
  def arrOfInt = array(field[Int]("PrimitiveFieldFourthLevelForArray"))
  def thirdDoc1 = doc[thirdLevelDocument1](tag)
  def thirdDoc2 = doc[thirdLevelDocument2](tag)
  def * = (x2, thirdDoc2) <> (second.tupled, second.unapply)
}

class thirdLevelDocument1(tag:Tag) extends SubDocument[third1](tag,"thirdLevelDocument1") {
//  type previousDocument = secoundLevelDocument
  def x3 = field[Int]("primitiveFieldThirdLevel")
  def dyn = field[Int]("DynamicprimitiveFieldThirdLevel")
  def arrayOfFurthDoc = doc[fourthLevelDocument](tag)
  def * = (x3, dyn) <> (third1.tupled, third1.unapply)
}

class thirdLevelDocument2(tag:Tag) extends SubDocument[third2](tag,"thirdLevelDocument2") {
//  type previousDocument = secoundLevelDocument
  def x3 = field[Double]("primitiveFieldThirdLevel11")
  def x4 = field[String]("primitiveFieldThirdLevel22")
  def * = (x3, x4) <> (third2.tupled, third2.unapply)
}


class fourthLevelDocument(tag:Tag) extends SubDocument[fourth](tag,"fourthLevelDocument") {
  def x4 = field[Int]("firstPrimitiveFieldFourthLevel")
  def x5 = field[Int]("sth")
  def * = (x4, x5) <> (fourth.tupled, fourth.unapply)
}


//class additional1(tag:Tag) extends SubDocument[fourth](tag,"add1") {
//  def x4 = field[Int]("firstPrimitiveFieldFourthLevel")
//  def arrOfInt = array(field[Int]("PrimitiveFieldFourthLevelForArray"))
//  def * = (x4, arrOfInt) <> (fourth.tupled, fourth.unapply)
//}
//
//
//class additional2(tag:Tag) extends SubDocument[fourth](tag,"add2") {
//  def x4 = field[Int]("firstPrimitiveFieldFourthLevel")
//  def arrOfInt = array(field[Int]("PrimitiveFieldFourthLevelForArray"))
//  def * = (x4, arrOfInt) <> (fourth.tupled, fourth.unapply)
//}
//
//case class noProj(c: Int, s: Int)
//
//
//class noProjection(tag:Tag) extends SubDocument[noProj](tag,"noProjectionDocument") {
//
//  def x4 = field[Int]("noProjectionField1")
//  def next =doc[fourthLevelDocument](tag)
//  def arrOfInt =field[Int]("noProjectionField2")
//
//  def * = (x4, arrOfInt) <> (noProj.tupled, noProj.unapply)
//}



class nestedStructureTest extends FunSuite with BeforeAndAfter with ScalaFutures {

  implicit override val patienceConfig = PatienceConfig(timeout = Span(50, Seconds))

  val documentQuery = TableQuery[firstLevelDocument]


  before {
    db = Database.forURL("mongodb://localhost:27017/test") // MongoDB binds to 127.0.0.1  in travis
  }

  var db: Database = _

  test("single value insert test")
  {
    val singleValueInsert = DBIO.seq(documentQuery +=
      first(4,second(1,third2(10,"iiii")))
    )
        lazy val result =  (db.run(singleValueInsert)).futureValue
    result
  }

  test("multiple value insert test")
  {
        val multipleValueInsert = DBIO.seq(documentQuery ++=
          List(
            first(4,second(1,third2(10,"iiii"))),
            first(0,second(1,third2(10,"mmmm"))),
            first(10,second(1,third2(10,"mmmm"))),
            first(14,second(1,third2(10,"mmmm")))
          ))

        lazy val result =  (db.run(multipleValueInsert)).futureValue
        result
  }

  test("third level document select")
  {
    lazy val result =  (db.run(documentQuery.map(x => x.secondDoc).result)).futureValue
    result.foreach(x => print(x))
  }

  test("two level field select ")
  {
    lazy val result = ( db.run(documentQuery.map(x => x.secondDoc.x2).result).map(println)).futureValue
    println("What kind of result " + result)
  }

  test("second level document select")
  {
    lazy  val result = ( db.run(documentQuery.map(x=>x.secondDoc.thirdDoc2).result)).futureValue
    result.map(x => print(x.asInstanceOf[third2].c + "    "))
  }


  test("filter by nested field")
  {
    print("adam")
    lazy val result = ( db.run(documentQuery.filter(x => x.x1 > 10 && x.x1 < 10).map(x => x.secondDoc.thirdDoc2).result)).futureValue
    println(result)
  }
}
