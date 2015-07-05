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


class ArrayDocument(tag: Tag)
  extends Document[(IndexedSeq[Int], IndexedSeq[IndexedSeq[Int]])](tag, "ArrayCollection") {

  def fourth: Query[Rep[Int], Int, IndexedSeq] = array(field[Int]("ss"))

  def fifth = array(array(field[Int]("someField")))

  def * = (fourth, fifth)

}


case class As(a: IndexedSeq[Int], b: IndexedSeq[Bs])

case class Bs(s: Int, c: Int)

/**
 *
 */
class ATables(tag: Tag) extends Document[As](tag, "tableA") {
  def g = array(field[Int]("someField"))

  def b = array(doc[BTables])

  def * = (g, b) <>(As.tupled, As.unapply)
}

class BTables(tag: Tag) extends Document[Bs](tag, "tableB") {
  def c = field[Int]("c")

  def d = field[Int]("d")

  def * = (c, d) <>(Bs.tupled, Bs.unapply)
}


class arrayTest extends FunSuite with BeforeAndAfter with ScalaFutures {
  implicit override val patienceConfig = PatienceConfig(timeout = Span(50, Seconds))


  val arrayQuery = TableQuery[ArrayDocument]
  val arrayNestedQuery = TableQuery[ATables]


  val simpleInsert = DBIO.seq(arrayQuery +=(IndexedSeq(1, 2, 3, 4), IndexedSeq(IndexedSeq(3, 4, 5), IndexedSeq(7, 8, 9))))

  val nestedInsert = DBIO.seq(arrayNestedQuery += (As(IndexedSeq(2), IndexedSeq(Bs(3, 6), Bs(7, 9)))))


  before {
    db = Database.forURL("mongodb://localhost:27017/test") // MongoDB binds to 127.0.0.1  in travis
  }

  var db: Database = _


  def primitiveArrayInsert = {
    db.run(simpleInsert)
  }.futureValue

  def nestedInsertAction = {
    db.run(nestedInsert)
  }.futureValue

  def primitiveArrayresult = {
    implicit def flatQueryShape[Level >: FlatShapeLevel <: ShapeLevel, T, Q <: QueryBase[_]](implicit ev: Q <:< Rep[T]) = RepShape[Level, Q, T]
    db.run(arrayQuery.map(x => x.fifth).result)
  }.futureValue

  def resultSimpleInsert = {
    implicit def flatQueryShape[Level >: FlatShapeLevel <: ShapeLevel, T, Q <: QueryBase[_]](implicit ev: Q <:< Rep[T]) = RepShape[Level, Q, T]

    (db.run(arrayQuery.map(x => x.fourth).result)).futureValue
  }


  test("array insert result") {
    primitiveArrayInsert
    //  nestedInsertAction


    Thread.sleep(100)
    val result = primitiveArrayresult

    assert(result.toString == "Vector(([ [ 3 , 4 , 5] , [ 7 , 8 , 9]]))")
  }


}
