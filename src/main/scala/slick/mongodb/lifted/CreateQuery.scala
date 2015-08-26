package slick.mongodb.lifted

import com.mongodb.DBObject
import com.mongodb.casbah.commons.Imports._
import com.mongodb.casbah.commons.MongoDBObject
import slick.ast._
import slick.mongodb.MongoQueryNode
import slick.mongodb.direct.MongoBackend

/**
 * Created by adam on 20.08.15.
 */


/**
 * Class extracted from LiftedMongoInvoker
 */
class CreateQuery {
import slick.mongodb.lifted.CreateQuery._

  def mongoQuery(tree:Node): Option[MongoDBObject] = tree match {
    case MongoQueryNode(_,f,s,fs,_,_,_)  =>
     val q = fs.foldLeft(new MongoDBObject) ((dbObject,node) => appendQueryParameterFromNode(node,dbObject))
    Some(q)
  }

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
    val attributeName = (arguments(0) match {case FwdPath(_ :: w)=>w}).iterator.mkString("",".","")

    val value = arguments(1) match {case LiteralNode(v)=>v}
    println(s"attributeName=$attributeName")
    println(s"value=$value")
    (attributeName,value)
  }

  def multipleArgumentFunctionParameters(arguments: Seq[Node]):(String,Seq[Any]) = {
    val attributeName = (arguments(0) match {case FwdPath(_ :: w)=>w}).iterator.mkString("",".","")
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
object CreateQuery {
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

  val supportedComparisonOperators = List(Library.<,Library.<=,Library.>,Library.>=)
  val SQLToMongoOperatorsMapping = Map(
    Library.< -> $lt,
    Library.<= -> $lte,
    Library.> -> $gt,
    Library.>= -> $gte
  )
}