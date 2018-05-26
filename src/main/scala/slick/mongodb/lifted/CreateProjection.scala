package slick.mongodb.lifted

import com.mongodb.casbah.commons.Imports._
import com.mongodb.casbah.commons.MongoDBObject
import slick.ast._
import slick.ast.TableNode
import slick.mongodb.MongoQueryNode
import slick.util.ConstArray

/**
 * Created by adam on 20.08.15.
 */


class CreateProjection {

  /** Class extracts parameters for projection*/
  def createProjection(tree:Node): Option[DBObject] = tree match {
    case MongoQueryNode(_,f,Pure(s,_),_,_,_,_)  =>

     val projections :ConstArray[Node]= s match {
        case p:ProductNode => p.children
        case s:Select => ConstArray(s)
        case r:Ref => ConstArray.empty
        case _ => ConstArray.empty
      }

      def singleArgumentFunctionParameters(argument: Node):(String,Any) = {
        val attributeName = (argument match {case FwdPath(_ :: w)=>w}).iterator.mkString("",".","")
        println(s"attributeName=$attributeName")
        (attributeName,1)
      }
      def addProjections (l:List[Node], q: DBObject):DBObject  = l match {
        case h :: t =>
          addProjections(t,q ++ singleArgumentFunctionParameters(h))
        case h:: Nil => q ++ singleArgumentFunctionParameters(h)
        case Nil => q
      }
      Some(addProjections(ConstArray.unapplySeq(projections).get.toList,new BasicDBObject()))
    case m:MongoQueryNode => Some(DBObject())
  }
}
