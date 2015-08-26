package slick.mongodb.lifted

import com.mongodb.casbah.commons.Imports._
import com.mongodb.casbah.commons.MongoDBObject
import slick.ast._
import slick.mongodb.MongoQueryNode

/**
 * Created by adam on 20.08.15.
 */
class CreateProjection {
  def createProjection(tree:Node): Option[MongoDBObject] = tree match {
    case MongoQueryNode(_,f,Pure(s,_),_,_,_,_)  =>

     val projections=  s match {
        case p:ProductNode => p.children
        case s:Select => List(s)
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
      Some(addProjections(projections.toList,new MongoDBObject()))
  }
}
