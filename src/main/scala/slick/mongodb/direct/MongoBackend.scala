package slick.mongodb.direct

import com.mongodb.casbah.Imports._
import com.mongodb.casbah.MongoClientURI
import com.typesafe.config.Config
import org.reactivestreams.Subscriber
import org.slf4j.LoggerFactory

import slick.backend.{RelationalBackend, DatabaseComponent}
import slick.util.{AsyncExecutor, SlickLogger}

trait MongoBackend extends RelationalBackend{
  protected[this] lazy val statementLogger = new SlickLogger(LoggerFactory.getLogger(classOf[MongoBackend].getName+".statement"))

  type This = MongoBackend
  type Database = DatabaseDef
  type Session = SessionDef
  type DatabaseFactory = DatabaseFactoryDef
  type StreamingContext = BasicStreamingActionContext  // TODO
  type Context = BasicActionContext //TODO

  val Database = new DatabaseFactoryDef {}
  val backend: MongoBackend = this

  /** Create a Database instance through [[https://github.com/typesafehub/config Typesafe Config]].
    * The supported config keys are backend-specific. This method is used by `DatabaseConfig`.
    * @param path The path in the configuration file for the database configuration, or an empty
    *             string for the top level of the `Config` object.
    * @param config The `Config` object to read from.
    */
  def createDatabase(config: Config, path: String) = ??? //TODO


  // TODO: add possibility to create DatabaseDef without url
  // In case user wants to create the connection with separate
  // parameters: username, password, etc. we don't really need to concatenate
  // them into URI and then pass it to MongoClientURI for parsing
  class DatabaseDef(val connectionUrl:String, val executor: AsyncExecutor) extends super.DatabaseDef{

    override def createSession(): Session = {
      val mongoUri = MongoClientURI(connectionUrl)
      val mongoClient = MongoClient(mongoUri)
      //TODO: check if there's better way without using Option.get:
      val mongoDb = mongoClient(mongoUri.database.get)
      new Session(mongoDb)
    }

    override def withTransaction[T](f: Session => T): T = throw new UnsupportedOperationException("Transactions are not supported by MongoDB")

    override def withDynTransaction[T](f: => T): T = throw new UnsupportedOperationException("Transactions are not supported by MongoDB")

    /** Free all resources allocated by Slick for this Database, blocking the current thread until
      * everything has been shut down.
      *
      * Backend implementations which are based on a naturally blocking shutdown procedure can
      * simply implement this method and get `shutdown` as an asynchronous wrapper for free. If
      * the underlying shutdown procedure is asynchronous, you should implement `shutdown` instead
      * and wrap it with `Await.result` in this method. */
    def close = ???  //TODO

    /** Return the default ExecutionContet for this Database which should be used for running
      * SynchronousDatabaseActions for asynchronous execution. */
    protected[this] def synchronousExecutionContext = executor.executionContext //TODO

    /** Create the default StreamingDatabaseActionContext for this backend. */
    protected[this] def createStreamingDatabaseActionContext[T](s: Subscriber[_ >: T], useSameThread: Boolean) = new BasicStreamingActionContext(s, useSameThread, DatabaseDef.this)//TODO

    /** Create the default DatabaseActionContext for this backend. */
    protected[this] def createDatabaseActionContext[T](_useSameThread: Boolean) = new BasicActionContext { val useSameThread = _useSameThread } // base on HeapBackend
  }

  trait DatabaseFactoryDef extends super.DatabaseFactoryDef{
    //TODO: add other methods and parameters here
    def forURL(url: String, executor: AsyncExecutor = AsyncExecutor.default()):DatabaseDef = new DatabaseDef(url, executor )
  }

  /**
   * Provides access to the Mongo database.
   *
   * @param mongoDb database session points to
   */
  class SessionDef(val mongoDb: MongoDB) extends super.SessionDef{
    def collectionByName(collectionName: String): MongoCollection = mongoDb(collectionName)

    /**
     * Inherited method
     *
     * MongoDB session does nothing when closing
     * since the database connection is shared between sessions
     *
     * */
    override def close(): Unit = {}

    /**
     * Inherited method
     *
     * Transactions are not supported by MongoDB
     * */
    override def rollback(): Unit = throw new UnsupportedOperationException("Transactions are not supported by MongoDB")

    /**
     * Inherited method
     *
     * Transactions are not supported by MongoDB
     * */
    override def withTransaction[T](f: => T): T = throw new UnsupportedOperationException("Transactions are not supported by MongoDB")

    /**
     * Inherited method
     *
     * Mongo sessions cannot be forced since MongoClient manages connections automatically
     */
    // TODO: discuss if we should throw an exception here or do nothing
    override def force(): Unit =
      throw new UnsupportedOperationException("Mongo session cannot be forced since MongoClient manages connections automatically")
  }

}

object MongoBackend extends MongoBackend





