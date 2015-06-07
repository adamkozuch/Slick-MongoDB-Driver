import com.typesafe.slick.testkit.tests.NestingTest
import com.typesafe.slick.testkit.util.{Testkit, DriverTest, RelationalTestDB}
import org.junit.Assert
import org.junit.runner.RunWith
import slick.jdbc.StaticQuery
import slick.mongodb.direct.MongoBackend
import slick.mongodb.lifted.MongoDriver

import scala.concurrent.ExecutionContext

/**
 * Created by adam on 02.06.15.
 */
@RunWith(classOf[Testkit])
class FirstTest extends DriverTest(FirstTest.tdb)

object FirstTest extends App{

val tdb = new RelationalTestDB {

  type Driver = MongoDriver


  val driver: Driver = MongoDriver
  val confName: String = "mongo"

  def dropUserArtifacts(implicit session: profile.Backend#Session) = {
//    for(t <- getLocalTables)
//      (Q.u+"drop table if exists "+driver.quoteIdentifier(t)+" cascade").execute
//    for(t <- getLocalSequences)
//      (Q.u+"drop sequence if exists "+driver.quoteIdentifier(t)+" cascade").execute
  }
  def assertTablesExist(tables: String*)(implicit session: profile.Backend#Session) {
//    for(t <- tables) {
//      try ((Q[Int]+"select 1 from "+driver.quoteIdentifier(t)+" where 1 < 0").buildColl[List]) catch { case _: Exception =>
//        Assert.fail("Table "+t+" should exist")
//      }
//    }
    Assert.assertSame(1,1)
  }
  def assertNotTablesExist(tables: String*)(implicit session: profile.Backend#Session) {
//    for(t <- tables) {
//      try {
//        (Q[Int]+"select 1 from "+driver.quoteIdentifier(t)+" where 1 < 0").buildColl[List]
//        Assert.fail("Table "+t+" should not exist")
//      } catch { case _: Exception => }
//    }
    Assert.assertSame(1,2)
  }
//  def tdb = new RelationalTestDB {
//    type Driver = MongoDriver
//    val driver: Driver = MongoDriver
//    val confName: String = "heap"
//    def createDB: profile.Backend#Database = profile.backend.Database(ExecutionContext.global)
//    def dropUserArtifacts(implicit session: profile.Backend#Session) {
//      val db = session.
//      db.getTables.foreach(t => db.dropTable(t.name))
//    }
//    def assertTablesExist(tables: String*)(implicit session: profile.Backend#Session) {
//      val all = session.database.getTables.map(_.name).toSet
//      for(t <- tables) {
//        if(!all.contains(t)) Assert.fail("Table "+t+" should exist")
//      }
//    }
//    def assertNotTablesExist(tables: String*)(implicit session: profile.Backend#Session) {
//      val all = session.database.getTables.map(_.name).toSet
//      for(t <- tables) {
//        if(all.contains(t)) Assert.fail("Table "+t+" should not exist")
//      }
//    }
//  }
  def createDB() = _
}}