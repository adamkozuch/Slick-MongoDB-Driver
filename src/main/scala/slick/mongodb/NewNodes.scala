package slick.mongodb

import slick.ast.Type._
import slick.ast._

import TypeUtil.typeToTypeUtil
import Util._

/**
 * Created by adam on 06.08.15.
 */

/** Node created for representing SubDocuments in AST.
  * It is removed by RemoveSubDocNodes phase and made a child of StructNode
  */
case class SubDocNode(termSymbol: TermSymbol, structNode: Node, select:Node,mapping:Node) extends Node {
  type Self = SubDocNode
  protected def buildType = ???
  def children = Seq(structNode,select, mapping)
  protected[this] def rebuild(ch: IndexedSeq[Node]) = SubDocNode(termSymbol,ch(0),ch(1),ch(2))
  protected[this] def withInferredType(scope: Scope, typeChildren: Boolean, retype: Boolean):Self = {
    val n2 = mapping.infer(scope,typeChildren, retype)
    SubDocNode(termSymbol, structNode,select,mapping) :@ n2.nodeType
  }
}



/**Node that will be used for representing collections in AST   */
case class CollectionNode(termSymbol: TermSymbol, node: Node)extends Node {
  type Self = CollectionNode

  protected def buildType = ??? //CollectionType(c,t)

  def children = Seq(node)

  protected[this] def rebuild(child:Node) = child

  protected[this] def rebuild(ch: IndexedSeq[Node]) = CollectionNode(termSymbol,ch(0))
  protected[this] def withInferredType(scope: Scope, typeChildren: Boolean, retype: Boolean) = ???
}


case class MongoQueryNode(sym:TermSymbol,from:Node ,select:Node ,filter:Option[Node] = None, sort:Seq[(Node, Ordering)] = Seq.empty,limit:Option[Node]=None,skip:Option[Node]=None ) extends DefNode
{
  type Self = MongoQueryNode

  val children = Seq(from, select) ++ filter ++ sort.map(_._1) ++ limit ++ skip

  def generators = Seq((sym, from))

  protected[this] def rebuildWithSymbols(gen: IndexedSeq[TermSymbol]) = copy(sym = gen.head)

  protected[this] def rebuild(ch: IndexedSeq[Node]) =
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
      sort = (sort, newSort).zipped.map { case ((_, o), n) => (n, o) },
      skip = newSkip.headOption,
      limit = newLimit.headOption
    )
  }


  def withInferredType(scope: Type.Scope, typeChildren: Boolean, retype: Boolean): Self = {
    val f2 = from.infer(scope, typeChildren, retype)
    val genScope = scope + (sym -> f2.nodeType.asCollectionType.elementType)

    val s2 = select.infer(genScope, typeChildren, retype)
    val fl2 = mapOrNone(filter)(_.infer(genScope, typeChildren, retype))
    val s = sort.map(_._1)
    val so2 = mapOrNone(s)(_.infer(genScope, typeChildren, retype))
    val l2 = mapOrNone(limit)(_.infer(genScope, typeChildren, retype))
    val sk2 = mapOrNone(skip)(_.infer(genScope, typeChildren, retype))

    val same = (f2 eq from) && (s2 eq select) && fl2.isEmpty  && l2.isEmpty && sk2.isEmpty

    val newType =
      if(!hasType || retype) CollectionType(f2.nodeType.asCollectionType.cons, s2.nodeType.asCollectionType.elementType)
      else nodeType

    if(same && newType == nodeType) this else {
      copy(
        from = f2,
        select = s2,
        filter = fl2.map(_.headOption).getOrElse(filter),
        sort = so2.map(so2 => (sort, so2).zipped.map { case ((_, o), n) => (n, o) }).getOrElse(sort),
        limit = l2.map(_.headOption).getOrElse(limit),
        skip = sk2.map(_.headOption).getOrElse(skip)) :@ newType
    }
  }
}





