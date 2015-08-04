package slick.mongodb.lifted

import com.mongodb.casbah.commons.MongoDBObject
import slick.SlickException
import slick.ast.TypeUtil.:@
import slick.backend.RelationalBackend
import slick.dbio.Effect.Schema
import slick.dbio.{Streaming, SynchronousDatabaseAction, Effect, NoStream}
import slick.jdbc.StreamingInvokerAction
import slick.memory.DistributedBackend
import slick.mongodb.compiler.{CreateResultConverter, ExpandDocument}

import slick.mongodb.types.DocumentComponent
import slick.relational.CompiledMapping
import slick.util.{SQLBuilder, DumpInfo}


import scala.language.{higherKinds, implicitConversions}
import slick.ast._
import slick.compiler.{CompilerState, Phase, QueryCompiler}
import slick.lifted.{QueryBase, RepShape, Query}
import slick.mongodb.direct.{GetResult, MongoQuery, MongoBackend}
import slick.profile.{FixedBasicStreamingAction, FixedBasicAction, RelationalDriver, RelationalProfile}

// TODO: split into traits?
trait MongoProfile extends RelationalProfile with MongoInsertInvokerComponent with MongoTypesComponent with MongoActionComponent with DocumentComponent with RelationalDriver
{ driver: MongoDriver =>

  type Backend = MongoBackend
  val backend = MongoBackend
  override val Implicit: Implicits = simple
  override val simple: SimpleQL  = new SimpleQL {}
  override val api = new API{}
  override val profile =this

  protected trait CommonImplicits extends super.CommonImplicits with ImplicitColumnTypes
//  trait Implicits extends super.Implicits with CommonImplicits
//  trait SimpleQL extends super.SimpleQL with Implicits
  trait API extends super.API with CommonImplicits
{
  type Document[T] = driver.Document[T]
  type SubDocument[T] = driver.SubDocument[T]

  //todo this shape should be move out from driver
  implicit def flatQueryShape[Level >: FlatShapeLevel <: ShapeLevel, T, Q <: QueryBase[_]](implicit ev: Q <:< Rep[T]) = RepShape[Level, Q, T]

}

  trait SimpleQL extends super.SimpleQL with Implicits


  // TODO: extend for complicated node structure, probably mongodb nodes should be used
  /** (Partially) compile an AST for insert operations */
  override def compileInsert(n: Node): CompiledInsert = n


  /** The compiler used for queries */
  override def queryCompiler: QueryCompiler = QueryCompiler.standard
  /** The compiler used for updates */
  override def updateCompiler: QueryCompiler = ???
  /** The compiler used for deleting data */
  override def deleteCompiler: QueryCompiler = ???
  /** The compiler used for inserting data */
  override def insertCompiler: QueryCompiler = ???

  trait ImplicitColumnTypes extends super.ImplicitColumnTypes{
    override implicit def charColumnType: BaseColumnType[Char] = ScalaBaseType.charType
    override implicit def longColumnType: BaseColumnType[Long] with NumericTypedType = ScalaBaseType.longType
    override implicit def byteColumnType: BaseColumnType[Byte] with NumericTypedType = ScalaBaseType.byteType
    override implicit def intColumnType: BaseColumnType[Int] with NumericTypedType = ScalaBaseType.intType
    override implicit def booleanColumnType: BaseColumnType[Boolean] = ScalaBaseType.booleanType
    override implicit def shortColumnType: BaseColumnType[Short] with NumericTypedType = ScalaBaseType.shortType
    override implicit def doubleColumnType: BaseColumnType[Double] with NumericTypedType = ScalaBaseType.doubleType
    override implicit def bigDecimalColumnType: BaseColumnType[BigDecimal] with NumericTypedType = ScalaBaseType.bigDecimalType
    override implicit def floatColumnType: BaseColumnType[Float] with NumericTypedType = ScalaBaseType.floatType
    override implicit def stringColumnType: BaseColumnType[String] = ScalaBaseType.stringType
  }

  trait Implicits extends super.Implicits with ImplicitColumnTypes{
    override implicit def ddlToDDLInvoker(d: SchemaDescription): DDLInvoker = createDDLInvoker(d)
    implicit def queryToLiftedMongoInvoker[T,C[_]](q: Query[_,T,C])(implicit session: MongoBackend#Session): LiftedMongoInvoker[T] =
      new LiftedMongoInvoker[T](queryCompiler.run(q.toNode).tree,session)
  }

/////////////////////////////////////////////////QueryExecutor//////////////////////////////////////////
  /** Create an executor -- this method should be implemented by drivers as needed */
  override type QueryExecutor[T] =  QueryExecutorDef[T]
  override def createQueryExecutor[R](tree: Node, param: Any): QueryExecutor[R] = ???


  // TODO: not required for MongoDB:
  /** Create a DDLInvoker -- this method should be implemented by drivers as needed */
  override def createDDLInvoker(ddl: SchemaDescription): DDLInvoker = throw new UnsupportedOperationException("Mongo driver doesn't support ddl operations.")

  override type SchemaDescription = SchemaDescriptionDef
  override def buildSequenceSchemaDescription(seq: Sequence[_]): SchemaDescription = ???
  override def buildTableSchemaDescription(table: Table[_]): SchemaDescription = ???

}

// TODO: make it a class?
trait MongoDriver extends MongoProfile

object MongoDriver extends MongoDriver
