package slick.mongodb

import slick.ast.Type._
import slick.ast._
import TypeUtil.typeToTypeUtil
import Util._
import slick.util.ConstArray

/**
 * Created by adam on 06.08.15.
 */

/** Node created for representing SubDocuments in AST.
  * It is removed by RemoveSubDocNodes phase and made a child of StructNode
  */
case class SubDocNode(termSymbol: TermSymbol, structNode: Node, select:Node,mapping:Node) extends Node {
  type Self = SubDocNode
  protected def buildType = ???
  def children = ConstArray(structNode,select, mapping)

  override protected[this] def rebuild(ch: ConstArray[Node]) = SubDocNode(termSymbol,ch(0),ch(1),ch(2))

  override protected[this] def withInferredType(scope: Scope, typeChildren: Boolean) = {
    val n2 = mapping.infer(scope,typeChildren)
    SubDocNode(termSymbol, structNode,select,mapping) :@ n2.nodeType
  }
}



/**Node that will be used for representing collections in AST   */
case class CollectionNode(termSymbol: TermSymbol, node: Node)extends Node {
  type Self = CollectionNode

  protected def buildType = ??? //CollectionType(c,t)

  def children = ConstArray(node)

  protected[this] def rebuild(child:Node) = child

  override protected[this] def rebuild(ch: ConstArray[Node]) =  CollectionNode(termSymbol,ch(0))

  override protected[this] def withInferredType(scope: Scope, typeChildren: Boolean) = ???
}


case class MongoQueryNode(sym:TermSymbol,from:Node ,select:Node ,filter:Option[Node] = None, sort:Seq[(Node, Ordering)] = Seq.empty,limit:Option[Node]=None,skip:Option[Node]=None ) extends DefNode
{
  type Self = MongoQueryNode

  val children = ConstArray(from, select) ++ filter //++ sort.map(_._1) ++ limit ++ skip

  def generators = ConstArray((sym, from))

  protected[this] def rebuildWithSymbols(gen: ConstArray[TermSymbol]) = copy(sym = gen.head)

  protected[this] def rebuild(ch: ConstArray[Node]) =
  {
    val newFrom = ch(0)
    val newSelect= ch(1)
    val filterOffset = 2
    val newFilter = ch.slice(filterOffset, filterOffset + filter.productArity)
    val sortOffset = filterOffset + newFilter.length
    val newSort = ch.slice(sortOffset,sortOffset+ sort.length)
    val skipOffset = sortOffset + newSort.length
    val newSkip = ch.slice(skipOffset,skipOffset+ skip.productArity)
    val limitOffset = skipOffset + newSkip.length
    val newLimit = ch.slice(limitOffset,limitOffset+ limit.productArity)

    copy(
      from = newFrom,
      select = newSelect,
      filter = newFilter.headOption,
      sort = (sort, newSort.toSeq).zipped.map { case ((_, o), n) => (n, o) },
      skip = newSkip.headOption,
      limit = newLimit.headOption
    )
  }


  def withInferredType(scope: Type.Scope, typeChildren: Boolean): Self = {
    val f2 = from.infer(scope, typeChildren)
    val genScope = scope + (sym -> f2.nodeType.asCollectionType.elementType)

    val s2 = select.infer(genScope, typeChildren)
    val fl2 = mapOrNone(filter)(_.infer(genScope, typeChildren))
    val s = sort.map(_._1)
//    val so2 = mapOrNone(s)(_.infer(genScope, typeChildren, retype))
    val l2 = mapOrNone(limit)(_.infer(genScope, typeChildren))
    val sk2 = mapOrNone(skip)(_.infer(genScope, typeChildren))

    val same = (f2 eq from) && (s2 eq select) && fl2.isEmpty  && l2.isEmpty && sk2.isEmpty

    val newType = nodeType
      //if(!hasType || retype) CollectionType(f2.nodeType.asCollectionType.cons, s2.nodeType.asCollectionType.elementType)
      //else nodeType

    this
//    if(same && newType == nodeType) this else {
//      copy(
//        from = f2,
//        select = s2,
//        filter = fl2.map(_.headOption).getOrElse(filter),
//        sort = so2.map(so2 => (sort, so2).zipped.map { case ((_, o), n) => (n, o) }).getOrElse(sort),
//        limit = l2.map(_.headOption).getOrElse(limit),
//        skip = sk2.map(_.headOption).getOrElse(skip)) :@ newType
//    }
  }
}





