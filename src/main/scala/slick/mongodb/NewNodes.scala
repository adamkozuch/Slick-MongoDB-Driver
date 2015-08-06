package slick.mongodb

import slick.ast.Type._
import slick.ast.{Node, TermSymbol}

/**
 * Created by adam on 06.08.15.
 */
class NewNodes {

}

case class SubDocNode(termSymbol: TermSymbol, node: Node)extends Node {
  type Self = SubDocNode

  protected def buildType = ???

  def children = Seq(node)

  protected[this] def rebuild(child:Node) = child

  protected[this] def rebuild(ch: IndexedSeq[Node]) = SubDocNode(termSymbol,ch(0))
  protected[this] def withInferredType(scope: Scope, typeChildren: Boolean, retype: Boolean) = ???
}


