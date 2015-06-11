import com.typesafe.slick.testkit.tests.NestingTest
import com.typesafe.slick.testkit.util.{TestDB, Testkit, DriverTest, RelationalTestDB}
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

object FirstTest {
  val tdb = new mongoTest
}

class mongoTest extends TestDB
{
  override  type Driver = MongoDriver
  override  val driver: Driver = MongoDriver

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

  override def createDB() =   profile.backend.Database.forURL("mongodb://localhost:27017/test")

}