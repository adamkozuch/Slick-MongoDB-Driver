import com.typesafe.slick.testkit.tests.NestingTest
import com.typesafe.slick.testkit.util.{Testkit, DriverTest, RelationalTestDB}
import org.junit.Assert
import org.junit.runner.RunWith
import slick.mongodb.lifted.MongoDriver

import scala.concurrent.ExecutionContext

/**
 * Created by adam on 02.06.15.
 */
@RunWith(classOf[Testkit])
class FirstTest extends DriverTest(FirstTest.tdb)

object FirstTest extends App{

val tdb = new RelationalTestDB {
  type Driver = this.type

  def dropUserArtifacts(implicit session: this.type#profile)

  def createDB() = ???

  val driver: Driver = _
  val confName: String = _
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
}