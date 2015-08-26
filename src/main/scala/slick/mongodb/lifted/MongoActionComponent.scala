package slick.mongodb.lifted

import com.mongodb.casbah.Imports._
import com.mongodb.casbah.MongoCollection
import com.mongodb.casbah.commons.MongoDBObject
import slick.SlickException
import slick.ast.Type.Structural
import slick.ast.TypeUtil.:@
import slick.ast._

import slick.dbio.{Streaming, SynchronousDatabaseAction, Effect, NoStream}
import slick.jdbc.JdbcBackend

import slick.mongodb.{MongoQueryNode, MongoInvoker}
import slick.mongodb.direct.{GetResult, TypedMongoCollection, TypedMongoCursor, MongoBackend}
import slick.profile.{FixedSqlAction, FixedBasicAction, FixedBasicStreamingAction, RelationalActionComponent}

import slick.util._

import scala.collection.mutable.Builder
import scala.util.control.NonFatal

import Util._


/**
 * Created by adam on 27.05.15.
 */
trait MongoActionComponent extends RelationalActionComponent {
  driver: MongoDriver =>

  type SchemaActionExtensionMethods = SchemaActionExtensionMethodsImpl
  type DriverAction[+R, +S <: NoStream, -E <: Effect] = FixedBasicAction[R, S, E]

  ///////////////////////////////////////////QueryActionExtension///////////////////////////
  //////////////////////////////////////////////////////////////////////////////////////////
  type QueryActionExtensionMethods[R, S <: NoStream] = QueryActionExtensionMethodsImpl[R, S]

  def createQueryActionExtensionMethods[R, S <: NoStream](tree: Node, param: Any): QueryActionExtensionMethods[R, S] =
    new QueryActionExtensionMethodsImpl[R, S](tree, param)

  class QueryActionExtensionMethodsImpl[R, S <: NoStream](tree: Node, param: Any) extends super.QueryActionExtensionMethodsImpl[R, S] {
    def result: DriverAction[R, S, Effect.Read] = {
      (tree match {
        case (mq : MongoQueryNode) :@ (ct: CollectionType) =>
           new MongoStreamingInvokerAction[R, Any, Effect] {
            streamingAction =>
            def run(ctx: MongoBackend#Context): R = {


              val q = new CreateQuery
              val p = new CreateProjection
              val  query = q.mongoQuery(mq)
              val projection = p.createProjection(mq)

              val conv  = new Converter[R](ct)

              val name  = mq.collect({case t:TableNode => t})(0).tableName
              val coll = new TypedMongoCollection[R](name)(ctx.session,conv.converter)
              val coursor =  coll.find(query.get,projection.get)
              
              val b = ct.cons.createBuilder(ct.elementType.classTag).asInstanceOf[Builder[R, R]]
              coursor.foreach(x => b += x)
              b.result()
            }
        override def createInvoker(session: MongoBackend#Session):LiftedMongoInvoker[R] = ??? //LiftedMongoInvoker[R](rsm)(session)
      }
        case First(rsm @ ResultSetMapping(_, compiled, _)) =>
          new SimpleDriverAction[R]("result") {
            def run(ctx: Backend#Context): R = {
              val liftedMongoInvoker = new LiftedMongoInvoker[R](rsm, ctx.session)
              liftedMongoInvoker.first(ctx.session)
            }
          }
      }).asInstanceOf[DriverAction[R, S, Effect.Read]]
    }

  }

  ////////////////////////////////////////////InsertActionExtension///////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////

  type InsertActionExtensionMethods[T] = InsertActionExtensionMethodsImpl[T]

  def createInsertActionExtensionMethods[T](compiled: MongoDriver.CompiledInsert) = new InsertActionExtensionMethodsImpl[T](compiled)

  class InsertActionExtensionMethodsImpl[T](compiled: CompiledInsert) extends super.InsertActionExtensionMethodsImpl[T] {
    //method taken from JDBC
    def wrapAction[E <: Effect, T](name: String, f: Backend#Session => Any): DriverAction[T, NoStream, E] =
      new SynchronousDatabaseAction[T, NoStream, Backend, E] with DriverAction[T, NoStream, E] {
        def run(ctx: Backend#Context) = f(ctx.session).asInstanceOf[T]

        override def getDumpInfo = ???

        //super.getDumpInfo.copy(name = name)
        def overrideStatements(_statements: Iterable[String]) =
          throw new SlickException("overrideStatements is not supported for insert operations")
      }

    val inv = createInsertInvoker[T](compiled)
    type SingleInsertResult = Unit
    type MultiInsertResult = Unit

    def +=(value: T) = wrapAction("+=", inv.+=(value)(_))

    def ++=(values: Iterable[T]) = wrapAction("+=", inv.++=(values)(_))
  }

  ///////////////////////////////////////////StreamingQueryActionExtension///////////////////////////
  //////////////////////////////////////////////////////////////////////////////////////////
  type StreamingQueryActionExtensionMethods[R, T] = StreamingQueryActionExtensionMethodsImpl[R, T]

  def createStreamingQueryActionExtensionMethods[R, T](tree: Node, param: Any): StreamingQueryActionExtensionMethods[R, T] =
    new StreamingQueryActionExtensionMethodsImpl[R, T](tree, param)

  class StreamingQueryActionExtensionMethodsImpl[R, T](tree: Node, param: Any) extends QueryActionExtensionMethodsImpl[R, Streaming[T]](tree, param) with super.StreamingQueryActionExtensionMethodsImpl[R, T] {
    override def result: StreamingDriverAction[R, T, Effect.Read] = super.result.asInstanceOf[StreamingDriverAction[R, T, Effect.Read]]
  }


  def createSchemaActionExtensionMethods(schema: SchemaDescription): SchemaActionExtensionMethods =
    ???

  //todo make it a class
  trait MongoStreamingInvokerAction[R, T, -E <: Effect] extends SynchronousDatabaseAction[R, Streaming[T], MongoBackend, E] with FixedBasicStreamingAction[R, T, E] {
    def createInvoker(session: MongoBackend#Session): MongoInvoker[R]

    type StreamState = CloseableIterator[R]


    override final def emitStream(ctx: MongoBackend#StreamingContext, limit: Long, state: StreamState): StreamState = {
      // todo passing session two times
      def iterator: TypedMongoCursor[R] = createInvoker(ctx.session).iterator(ctx.session)
      val it = if (state ne null) state else iterator
      var count = 0L
      try {
        while (if (it.hasNext) it.hasNext && count < limit else count < limit && it.hasNext) {
          count += 1
          ctx.emit(it.next())
        }
      }
      catch {
        case NonFatal(ex) =>
          try it.close() catch ignoreFollowOnError
          throw ex
      }
      // todo probalby implementing sth like buffer
      if (it.hasNext) it else null
    }

    /** Create an Action that returns only the first value of this stream of data as an `Option`.
      * Only available on streaming Actions. */
    def headOption =  ???  //new HeadOptionAction()

    /** Create an Action that returns only the first value of this stream of data. The Action will
      * fail if the stream is empty. Only available on streaming Actions. */
    def head = ??? //new HeadAction()

    /** Return the name, main info, attribute info and named children */
    def getDumpInfo = ???


//    private[this] class HeadAction extends SynchronousDatabaseAction[T, NoStream, MongoBackend, E] with FixedBasicAction[R, T, E] {
//      def run(ctx: MongoBackend#Context): R = createInvoker(ctx.session).first(ctx.session)
//      def getDumpInfo = ???
//    }
//
//    private[this] class HeadOptionAction extends SynchronousDatabaseAction[Option[T], NoStream, MongoBackend, E] with FixedBasicAction[R, T, E] {
//      def run(ctx: MongoBackend#Context): Option[R] = createInvoker(ctx.session).firstOption(ctx.session)
//      def getDumpInfo = ???
//    }

  }



  abstract class SimpleDriverAction[+R](name: String) extends SynchronousDatabaseAction[R, NoStream, Backend, Effect] with DriverAction[R, NoStream, Effect] {
    type StreamState = this.type

    /** Run this action synchronously and produce a result, or throw an Exception to indicate a
      * failure. */

    /** Return the name, main info, attribute info and named children */
    def getDumpInfo = ???
  }

}





