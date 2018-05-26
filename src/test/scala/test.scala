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
case class top1(x: Int, secondLevel: second, arr: IndexedSeq[Double]) extends doc.NewTerm

case class top2(secondLevel: second) extends doc.NewTerm

case class second(c: Int, thirdLevel1: third) extends doc.NewTerm

case class third(c: Double, s: IndexedSeq[String]) extends doc.NewTerm

class topLevel1(tags: Tag) extends Document[top1](tags, "topLevel1") {
  def x1 = field[Int]("primitiveFieldFirstLevel")

  def secondDoc = doc[secoundLevelDocument](tags)

  def arrOfDouble = array(field[Double]("doubleArray"))

  def * = (x1, secondDoc, arrOfDouble) <> (top1.tupled, top1.unapply) // tutaj chodzi o to że dokument jest nested
}

class topLevel2(tags: Tag) extends Document[top2](tags, "topLevel2") {
  def secondDoc = doc[secoundLevelDocument](tags)

  def * = (secondDoc) <> (top2, top2.unapply) // tutaj chodzi o to że dokument jest nested
}

class secoundLevelDocument(tag: Tag) extends SubDocument[second](tag, "secoundLevelDocument") {
  def x2 = field[Int]("primitiveFieldSecundLevel")

  def arrOfInt = array(field[Int]("PrimitiveFieldFourthLevelForArray"))

  def thirdDoc = doc[thirdLevelDocument](tag)

  def * = (x2, thirdDoc) <> (second.tupled, second.unapply)
}

class thirdLevelDocument(tag: Tag) extends SubDocument[third](tag, "thirdLevelDocument") {
  def x3 = field[Double]("primitiveFieldThirdLevel11")

  def x4 = array(field[String]("array of primitives"))

  def * = (x3, x4) <> (third.tupled, third.unapply)
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

  val documentQuery1 = TableQuery[topLevel1]
  val documentQuery2 = TableQuery[topLevel2]


  before {
    db = Database.forURL("mongodb://localhost:27017/test") // MongoDB binds to 127.0.0.1  in travis
  }

  var db: Database = _

  test("single value insert test 1") {
    val singleValueInsert = DBIO.seq(documentQuery1 +=
      top1(1, second(1, third(10, IndexedSeq("John", "Smith"))), IndexedSeq(2.3, 7.8, 9.1))
    )
    lazy val result = (db.run(singleValueInsert)).futureValue
    result
  }

  test("single value insert test 2") {
    val singleValueInsert = DBIO.seq(documentQuery2 +=
      top2(second(1, third(111, IndexedSeq("Adam", "Kozuch"))))
    )
    lazy val result = (db.run(singleValueInsert)).futureValue
    result
  }

  test("multiple value insert test 1 - document top1") {
    val multipleValueInsert = DBIO.seq(documentQuery1 ++=
      List(
        top1(100, second(5, third(10, IndexedSeq("ABC", "DEF"))), IndexedSeq(4.5, 3.0, 8.8, 1.6)),
        top1(145, second(7, third(19, IndexedSeq("IJK", "LMN"))), IndexedSeq(4.5, 9.0, 3.8, 7.6))
      ))

    lazy val result = (db.run(multipleValueInsert)).futureValue
    result
  }

  test("multiple value insert test 2 - document top2") {
    val multipleValueInsert = DBIO.seq(documentQuery2 ++=
      List(
        top2(second(5, third(10, IndexedSeq("AAAA", "BBB")))),
        top2(second(7, third(19, IndexedSeq("CCC", "DDD"))))
      ))

    lazy val result = (db.run(multipleValueInsert)).futureValue
    result
  }


  test("topLevel1 whole document no conditions") {
    val expected = Vector(
      top1(1, second(1, third(10, IndexedSeq("John", "Smith"))), IndexedSeq(2.3, 7.8, 9.1)),
      top1(100, second(5, third(10, IndexedSeq("ABC", "DEF"))), IndexedSeq(4.5, 3.0, 8.8, 1.6)),
      top1(145, second(7, third(19, IndexedSeq("IJK", "LMN"))), IndexedSeq(4.5, 9.0, 3.8, 7.6)))
    lazy val result = (db.run(documentQuery1.result)).futureValue

    assert(result == expected)
  }

  test("topLevel1 query array from top level") {
    val expected = Vector(
      IndexedSeq(2.3, 7.8, 9.1)
      , IndexedSeq(4.5, 3.0, 8.8, 1.6)
      , IndexedSeq(4.5, 9.0, 3.8, 7.6))
    lazy val result = (db.run(documentQuery1.map(x => x.arrOfDouble).result)).futureValue
    print(result)

    assert(result == expected)
  }

  test("topLevel1 query primitive value") {
    val expected = Vector(1, 100, 145)
    lazy val result = (db.run(documentQuery1.map(x => x.x1).result)).futureValue
    print(result)

    assert(result == expected)
  }

  test("topLevel1 query nested document") {
    val expected = Vector(
      second(1, third(10, IndexedSeq("John", "Smith"))),
      second(5, third(10, IndexedSeq("ABC", "DEF"))),
      second(7, third(19, IndexedSeq("IJK", "LMN"))))
    lazy val result = (db.run(documentQuery1.map(x => x.secondDoc).result)).futureValue
    print(result)

    assert(result == expected)
  }

  test("topLevel1 checking number of documnents after filter") {
    lazy val result = (db.run(documentQuery1.filter(x => x.secondDoc.x2 <= 5).map(x => x.arrOfDouble).result)).futureValue
    print(result)

    assert(result.length == 2)
  }

  //  test("topLevel2 whole document no conditions (chaninig case)")
  //  {
  //    val expected = Vector(
  //      top2(second(1,third2(111,IndexedSeq("Adam", "Kozuch")))),
  //      top2(second(5,third2(10,IndexedSeq("AAAA", "BBB")))),
  //      top2(second(7,third2(19,IndexedSeq("CCC", "DDD")))))
  //    lazy val result =  (db.run(documentQuery2.result)).futureValue
  //
  //    assert(result == expected)
  //  }
}
