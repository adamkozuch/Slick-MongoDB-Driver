package slick.mongodb


import slick.backend.DatabasePublisher
import slick.lifted.{ForeignKeyQuery, ProvenShape}
import slick.mongodb.lifted.MongoDriver.api._

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

import scala.slick.driver

// A Suppliers table with 6 columns: id, name, street, city, state, zip
class Suppliers(tag: Tag)
  extends Table[(Int, String, String, String, String, String)](tag, "SUPPLIERS") {

  // This is the primary key column:
  def id: Rep[Int] = column[Int]("SUP_ID", O.PrimaryKey)
  def name: Rep[String] = column[String]("SUP_NAME")
  def street: Rep[String] = column[String]("STREET")
  def city: Rep[String] = column[String]("CITY")
  def state: Rep[String] = column[String]("STATE")
  def zip: Rep[String] = column[String]("ZIP")


  // Every table needs a * projection with the same type as the table's type parameter
  def * : ProvenShape[(Int, String, String, String, String, String)] =
    (id, name, street, city, state, zip)
}

// A Coffees table with 5 columns: name, supplier id, price, sales, total
class Coffees(tag: Tag)
  extends Table[(String, Int, Double, Int, Int)](tag, "COFFEES") {

  def name: Rep[String] = column[String]("COF_NAME", O.PrimaryKey)
  def supID: Rep[Int] = column[Int]("SUP_ID")
  def price: Rep[Double] = column[Double]("PRICE")
  def sales: Rep[Int] = column[Int]("SALES")
  def total: Rep[Int] = column[Int]("TOTAL")

  def * : ProvenShape[(String, Int, Double, Int, Int)] =
    (name, supID, price, sales, total)


}

//
//class Next(tag: Tag)
//  extends Document[(String, Int, Double, Int, Int)](tag, "COFFEES") {
//
//  def name: Rep[String] = column[String]("COF_NAME", O.PrimaryKey)
//  def supID = column[Int]("SUP_ID")
//  def price: Rep[Double] = column[Double]("PRICE")
//  def sales: Rep[Int] = column[Int]("SALES")
//  def total: Rep[Int] = column[Int]("TOTAL")
//
//  def * : ProvenShape[(String, Int, Double, Int, Int)] =
//    (name, supID, price, sales, total)
//
//
//}

//class Sample(tag: Tag)
//  extends Document[(String,Int,  Double,Int, Option[(Int,String)])]     (tag, "COFFEES") {
//
//  def name: Rep[String] = column[String]("COF_NAME")
//  def c :Rep[Option[Option[(Int,String)]]] = document[Option[(Int,String)]]("COF_NAME")
//  def price: Rep[Double] = column[Double]("PRICE")
//  def sales: Rep[Int] = column[Int]("SALES")
//  def total: Rep[Int] = column[Int]("TOTAL")
//  def supID: Rep[Int] = column[Int]("SUP_ID")
//
//
//  def * : ProvenShape[(String,Int, Double, Int, Option[Option[(Int,String)]])] =
//    (name,supID, price, sales, c)
//
//
//}
//class Sample(tag: Tag)
//  extends Document[(String,Int,  Double,Int, Int)]  (tag, "COFFEES") {
//
//  def name: Rep[String] = column[String]("COF_NAME")
//  def c  = document[Int]("COF_NAME")
//  def price: Rep[Double] = column[Double]("PRICE")
//  def sales: Rep[Int] = column[Int]("SALES")
//  def total: Rep[Int] = column[Int]("TOTAL")
//  def supID: Rep[Int] = column[Int]("SUP_ID")
//
//
//  def * : ProvenShape[(String,Int, Double, Int, Option[Int])] =
//    (name,supID, price, sales, c)
//
//
//}



object example extends App {
  val db = Database.forURL("mongodb://localhost:27017/test")
  try {
    // The query interface for the Suppliers table
    val suppliers: TableQuery[Suppliers] = TableQuery[Suppliers]

    // the query interface for the CoffeCompiledMapping(_, elemType)es table
    val coffees: TableQuery[Coffees] = TableQuery[Coffees]

//
//    val setupAction: DBIO[Unit] = DBIO.seq(
      // Create the schema by combining the DDLs for the Suppliers and Coffees
      // tables using the query interfaces


      // Insert some suppliers
      //    suppliers += (101, "Acme, Inc.", "99 Market Street", "Groundsville", "CA", "95199"),
      //    suppliers += ( 49, "Superior Coffee", "1 Party Place", "Mendocino", "CA", "95460"),
      //    suppliers += (150, "The High Ground", "100 Coffee Lane", "Meadows", "CA", "93966")
//    )
//
//    val setupFuture: Future[Unit] = db.run(setupAction)
//
//    val f = setupFuture.flatMap { _ =>
//
//      // Insert some coffees (using JDBC's batch insert feature)
//      val insertAction: DBIO[Option[Int]] =
//        coffees ++= Seq(//
//          ("Colombian", 101, 7.99, 0, 0),
//          ("French_Roast", 49, 8.99, 0, 0),
//          ("Espresso", 150, 9.99, 0, 0),
//          ("Colombian_Decaf", 101, 8.99, 0, 0),
//          ("French_Roast_Decaf", 49, 9.99, 0, 0)
//        )
//
//      db.run(insertAction)
//
//          val insertAndPrintAction: DBIO[Unit] = insertAction.map { coffeesInsertResult =>
//            // Print the number of rows inserted
//            coffeesInsertResult foreach { numRows =>
//              println(s"Inserted $numRows rows into the Coffees table")
//            }
//          }
//
//      val allSuppliersAction: DBIO[Seq[(Int, String, String, String, String, String)]] =
//        suppliers.result
//
//      //    val combinedAction: DBIO[Seq[(Int, String, String, String, String, String)]] =
//      //      insertAndPrintAction >> allSuppliersAction
//
//      val combinedFuture: Future[Seq[(Int, String, String, String, String, String)]] =
//        db.run(allSuppliersAction)
//
//      combinedFuture.map { allSuppliers =>
//        allSuppliers.foreach(println)
//      }
//
//    }
//      .flatMap { _ =>
//
//      /* Streaming */
//
//      val coffeeNamesAction: StreamingDBIO[Seq[String], String] =
//        coffees.map(_.name).result
//
//      val coffeeNamesPublisher: DatabasePublisher[String] =
//        db.stream(coffeeNamesAction)
//
//      coffeeNamesPublisher.foreach(println)
//
//    }
//      .flatMap { _ =>
//
//      /* Filtering / Where */
//      println("//////////////////////////////////////////////////////////")
//      // Construct a query where the price of Coffees is > 9.0
//      val filterQuery: Query[Sample, (String, Int, Double, Int, Int), Seq] =
//        coffees.filter(_.price > 9.0)
//
//
//
//      // Execute the query and print the Seq of results
//      db.run(filterQuery.result.map(println)) // todo not sure if this should be a vector
//
//    }
//    Await.result(f, Duration.Inf)


  }
}