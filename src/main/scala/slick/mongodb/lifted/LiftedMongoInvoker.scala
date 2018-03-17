package slick.mongodb.lifted

import com.mongodb.DBObject
import com.mongodb.casbah.commons.Imports._

import slick.ast._
import slick.mongodb.MongoInvoker
import slick.mongodb.direct.{GetResult, MongoBackend, TypedMongoCollection}

trait GenericLiftedMongoInvoker[T]  {
  protected def queryNode: Node

  /** Used to retrieve attribute names from the query node */
  protected def attributeNames = attributes  //without type

  def expand(n:Node):List[String] = n match {
    case s:StructNode  => {
      val attr :List[String]= s.elements.flatMap(x =>  x._2 match {
        case nested:StructNode =>  List(x._1.name) ++ expand(nested)
        case p:ProductNode => p.children.map(_.asInstanceOf[Select].field.name).toList
        case field:Select => List(field.field.name)
      }
      ).toList
      attr
    }
    case pn:ProductNode  =>  pn.children.map(_.asInstanceOf[Select].field.name).toList
    case d:Select => List(d.field.name)
  }
  /** Used to retrieve attribute names and their types from the query node*/
  protected def attributes[String] = queryNode match {  // todo for now result without a typ
    case TableExpansion(_,_,s:StructNode) =>  expand(s)
    case TableExpansion(_,_,pn: ProductNode) => pn.children.map(_.asInstanceOf[Select].field.name).toIndexedSeq  //zip pn.nodeChildren.map( ch =>ch.nodeType) //zip pn.buildType.children  // todo build type is now protected resolve problem
    case TableExpansion(_,_,TypeMapping(ch,_,_)) =>  ch.children.map(_.asInstanceOf[Select].field.name)
  }

  /** Used to convert data from DBObject to specified type after find operation - required for TypedMongoCollection creation */
  val converter: GetResult[Product] =
  // TODO: add support for arbitrary type, non-tuple (single attribute)
    GetResult[Product](r => {
      val merged = attributeNames.map(x => x)     //r.get(_).get)
      GenericLiftedMongoInvoker.seqToTuple(merged)
    })

  protected def collection(session: MongoBackend#Session) = cachedCollection match{
    case Some(c) => c
    case None =>
      cachedCollection = Some(newCollection(session))
      cachedCollection.get
  }
  private var cachedCollection: Option[TypedMongoCollection[Product]] = None
  private def newCollection(session: MongoBackend#Session): TypedMongoCollection[Product] = queryNode match{
    case te: TableExpansion =>
      val collectionName = te.table.asInstanceOf[TableNode].tableName
      new TypedMongoCollection[Product](collectionName)(session,converter)
    case _ => throw new IllegalArgumentException("Only nodes of type TableExpansion supported")
  }
}

object GenericLiftedMongoInvoker{
  def seqToTuple(s:Seq[Any]):Product = s.size match {
    case 0 => throw new IllegalArgumentException("Number of arguments returned may not be 0")
    case 1 => new Tuple1[Any](s(0))
    case 2 => seqToTuple2(s)
    case 3 => seqToTuple3(s)
    case 4 => seqToTuple4(s)
    case 5 => seqToTuple5(s)
    case 6 => seqToTuple6(s)
    case 7 => seqToTuple7(s)
    case 8 => seqToTuple8(s)
    case 9 => seqToTuple9(s)
    case 10 => seqToTuple10(s)
    case 11 => seqToTuple11(s)
    case 12 => seqToTuple12(s)
    case 13 => seqToTuple13(s)
    case 14 => seqToTuple14(s)
    case 15 => seqToTuple15(s)
    case 16 => seqToTuple16(s)
    case 17 => seqToTuple17(s)
    case 18 => seqToTuple18(s)
    case 19 => seqToTuple19(s)
    case 20 => seqToTuple20(s)
    case 21 => seqToTuple21(s)
    case 22 => seqToTuple22(s)
    case _ => throw new IllegalArgumentException("Only tuple query return types supported")
  }

  def seqToTuple2(s:Seq[Any]):(Any, Any) = (s(0),s(1))
  def seqToTuple3(s:Seq[Any]):(Any, Any, Any) = (s(0),s(1),s(2))
  def seqToTuple4(s:Seq[Any]):(Any, Any, Any, Any) = (s(0),s(1),s(2),s(3))
  def seqToTuple5(s:Seq[Any]):(Any, Any, Any, Any, Any) = (s(0),s(1),s(2),s(3),s(4))
  def seqToTuple6(s:Seq[Any]):(Any, Any, Any, Any, Any, Any) = (s(0),s(1),s(2),s(3),s(4),s(5))
  def seqToTuple7(s:Seq[Any]):(Any, Any, Any, Any, Any, Any, Any) = (s(0),s(1),s(2),s(3),s(4),s(5),s(6))
  def seqToTuple8(s:Seq[Any]):(Any, Any, Any, Any, Any, Any, Any, Any) = (s(0),s(1),s(2),s(3),s(4),s(5),s(6),s(7))
  def seqToTuple9(s:Seq[Any]):(Any, Any, Any, Any, Any, Any, Any, Any, Any) = (s(0),s(1),s(2),s(3),s(4),s(5),s(6),s(7),s(8))
  def seqToTuple10(s:Seq[Any]):(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) = (s(0),s(1),s(2),s(3),s(4),s(5),s(6),s(7),s(8),s(9))
  def seqToTuple11(s:Seq[Any]):(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) = (s(0),s(1),s(2),s(3),s(4),s(5),s(6),s(7),s(8),s(9),s(10))
  def seqToTuple12(s:Seq[Any]):(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) = (s(0),s(1),s(2),s(3),s(4),s(5),s(6),s(7),s(8),s(9),s(10),s(11))
  def seqToTuple13(s:Seq[Any]):(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) = (s(0),s(1),s(2),s(3),s(4),s(5),s(6),s(7),s(8),s(9),s(10),s(11),s(12))
  def seqToTuple14(s:Seq[Any]):(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) = (s(0),s(1),s(2),s(3),s(4),s(5),s(6),s(7),s(8),s(9),s(10),s(11),s(12),s(13))
  def seqToTuple15(s:Seq[Any]):(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) = (s(0),s(1),s(2),s(3),s(4),s(5),s(6),s(7),s(8),s(9),s(10),s(11),s(12),s(13),s(14))
  def seqToTuple16(s:Seq[Any]):(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) = (s(0),s(1),s(2),s(3),s(4),s(5),s(6),s(7),s(8),s(9),s(10),s(11),s(12),s(13),s(14),s(15))
  def seqToTuple17(s:Seq[Any]):(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) = (s(0),s(1),s(2),s(3),s(4),s(5),s(6),s(7),s(8),s(9),s(10),s(11),s(12),s(13),s(14),s(15),s(16))
  def seqToTuple18(s:Seq[Any]):(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) = (s(0),s(1),s(2),s(3),s(4),s(5),s(6),s(7),s(8),s(9),s(10),s(11),s(12),s(13),s(14),s(15),s(16),s(17))
  def seqToTuple19(s:Seq[Any]):(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) = (s(0),s(1),s(2),s(3),s(4),s(5),s(6),s(7),s(8),s(9),s(10),s(11),s(12),s(13),s(14),s(15),s(16),s(17),s(18))
  def seqToTuple20(s:Seq[Any]):(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) = (s(0),s(1),s(2),s(3),s(4),s(5),s(6),s(7),s(8),s(9),s(10),s(11),s(12),s(13),s(14),s(15),s(16),s(17),s(18),s(19))
  def seqToTuple21(s:Seq[Any]):(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) = (s(0),s(1),s(2),s(3),s(4),s(5),s(6),s(7),s(8),s(9),s(10),s(11),s(12),s(13),s(14),s(15),s(16),s(17),s(18),s(19),s(20))
  def seqToTuple22(s:Seq[Any]):(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) = (s(0),s(1),s(2),s(3),s(4),s(5),s(6),s(7),s(8),s(9),s(10),s(11),s(12),s(13),s(14),s(15),s(16),s(17),s(18),s(19),s(20),s(21))
}


// TODO: use MongoNode
class LiftedMongoInvoker[T](val queryNode: Node, val session: MongoBackend#Session) extends MongoInvoker[T] with GenericLiftedMongoInvoker[T]{
  import slick.mongodb.lifted.LiftedMongoInvoker._
  println(s"Query invoker created with node:\t$queryNode")


  // TODO: make `collection(session)` return `TypedMongoCollection[T]`, not `TypedMongoCollection[Product]`
  override lazy val mongoCollection: TypedMongoCollection[T] = collection(session).asInstanceOf[TypedMongoCollection[T]]
  override lazy val query: Option[DBObject] = ??? //mongoQuery.map(_.underlying)

  val comprehension = {

    queryNode.asInstanceOf[ResultSetMapping].left
  }

  // TODO: add support for other operators here
  // TODO: does it make sense to use query builder instead of `query: MongoDBObject` here ?
  def appendQueryParameterFromNode(node: Node,query: MongoDBObject): MongoDBObject =  node match {
    case Apply(Library.And,arguments) =>
      arguments.foldLeft(query){(dbObject,node) => appendQueryParameterFromNode(node,dbObject)}
    case Apply(Library.Or,arguments) =>
      val orClauses = arguments.map{node => appendQueryParameterFromNode(node,new MongoDBObject)}
      query.++(($or,orClauses))
    case Apply(Library.==,arguments) =>
      val (attributeName,value) = singleArgumentFunctionParameters(arguments)
      query.++((attributeName,value))
    case Apply(Library.In,arguments) =>
      val (attributeName, parameters) = multipleArgumentFunctionParameters(arguments)
      query.++((attributeName,MongoDBObject($in -> parameters)))
    case Apply(operator: Library.SqlOperator,arguments) if supportedComparisonOperators.contains(operator) =>
      val (attributeName,value) = singleArgumentFunctionParameters(arguments)
      val comparisonOperator = SQLToMongoOperatorsMapping(operator)
      addComplexAttribute(query,attributeName,MongoDBObject(comparisonOperator -> value))
    case Apply(operatorNot: Library.SqlOperator,nArguments) if operatorNot == Library.Not && nArguments.size == 1 =>
      appendNegatedParameterFromNode(nArguments(0),query)
  }

  def appendNegatedParameterFromNode(node: Node, query: DBObject):MongoDBObject = node match {
    case Apply(Library.And,arguments) =>
      val andClauses:MongoDBObject = appendQueryParameterFromNode(node,new MongoDBObject)
      val negatedIterator = andClauses.underlying.iterator.asInstanceOf[Iterator[(String,Any)]].map { case (attributeName, value) => (attributeName, MongoDBObject($not -> value))}
      val builder  = MongoDBList.newBuilder
      for (document <- negatedIterator) builder += MongoDBObject(document)
      val negated = builder.result()
      query.++(($or,negated))
    case Apply(Library.Or,arguments) =>
      val orClauses = arguments.map{node => appendQueryParameterFromNode(node,new MongoDBObject)}
      query.++(($nor,orClauses))
    case Apply(Library.==,arguments) =>
      val (attributeName,value) = singleArgumentFunctionParameters(arguments)
      query.++((attributeName,MongoDBObject($ne -> value)))
    case Apply(Library.In,arguments) =>
      val (attributeName,parameters) = multipleArgumentFunctionParameters(arguments)
      query.++((attributeName,MongoDBObject($nin -> parameters)))
    case Apply(operator: Library.SqlOperator,arguments) if supportedComparisonOperators.contains(operator) =>
      val (attributeName,value) = singleArgumentFunctionParameters(arguments)
      val comparisonOperator = SQLToMongoOperatorsMapping(operator)
      query.++((attributeName,MongoDBObject($not -> MongoDBObject(comparisonOperator -> value))))
    case Apply(Library.Not,arguments) if arguments.size == 1 =>
      appendQueryParameterFromNode(arguments(0),query)
  }

  def singleArgumentFunctionParameters(arguments: Seq[Node]):(String,Any) = {
    val attributeName = (arguments(0) match {case Path(an :: _)=>an}).toString
    val value = arguments(1) match {case LiteralNode(v)=>v}
    println(s"attributeName=$attributeName")
    println(s"value=$value")
    (attributeName,value)
  }
  
  def multipleArgumentFunctionParameters(arguments: Seq[Node]):(String,Seq[Any]) = {
    val attributeName = (arguments(0) match {case Path(an :: _)=>an}).toString
    val parameters = arguments(1).asInstanceOf[ProductNode].children.map {case LiteralNode(v)=>v}
    println(s"attributeName=$attributeName")
    println(s"value=$parameters")
    (attributeName,parameters)
  }

  def addComplexAttribute(dbObject: MongoDBObject,attributeName: String,value: MongoDBObject):MongoDBObject = dbObject.get(attributeName) match{
    case None => dbObject += ((attributeName,value))
    case Some(existing:DBObject) => dbObject += ((attributeName,{existing.putAll(value);existing}))
    case Some(x) => println(s"$x of type ${x.getClass}"); ???
  }

}
object LiftedMongoInvoker{
  val $ne = "$ne"
  val $lt = "$lt"
  val $lte = "$lte"
  val $gt = "$gt"
  val $gte = "$gte"
  val $or = "$or"
  val $in = "$in"
  val $nin = "$nin"
  val $nor = "$nor"
  val $not = "$not"
  def apply[T](tree : Node)(implicit session:MongoBackend#Session) = new LiftedMongoInvoker[T](tree,session)

  val supportedComparisonOperators = List(Library.<,Library.<=,Library.>,Library.>=)
  val SQLToMongoOperatorsMapping = Map(
    Library.< -> $lt,
    Library.<= -> $lte,
    Library.> -> $gt,
    Library.>= -> $gte
  )
}