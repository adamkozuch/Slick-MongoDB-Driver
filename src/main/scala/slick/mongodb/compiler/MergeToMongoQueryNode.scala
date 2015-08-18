package slick.mongodb.compiler

import slick.ast._
import slick.compiler.{WellTyped, Phase, CompilerState}
import Util._
import TypeUtil._
import slick.mongodb.MongoQueryNode


/**
 * Created by adam on 14.08.15.
 */
class mergeToMongoQueryNode extends Phase {
  type State = this.type
  val name: String = "mergeToMongoQueryNode"
  def apply(state: CompilerState) = state.map { n => ClientSideOp.mapServerSide(n) { tree =>


    //todo think about better solution
    val from = tree.collect({case t:TableNode =>t})(0)
    val select = tree.collect({case Bind(g,t,p:Pure) =>p})(0)
    val filter = tree.collect({case Filter(g,f,w) => w})
    val f1 =if(filter.nonEmpty) Option(filter(0)) else None

    val sort   = tree.collect({case SortBy(g,f,b) => b})
    val s1 = if(sort.isEmpty) Seq.empty else sort(0)



//    /**
//     *How to collect nodes in linear way
//     */
//    def convert(tree:Node):Node =tree match
//    {
//      case Filter(_,f,w) =>  convert(f)
//      case Bind(_,f,s) => convert(f)
//      case Take(f,c) => convert(f)
//      case SortBy(_,f,by) => convert(f)
//      case Drop(f,c) => convert(c)
//
//    }

    /**
     * Think If I should replace all references
     */
      MongoQueryNode(new AnonSymbol, from, select,f1,s1).infer(typeChildren = true)
}}}
