package slick.mongodb.compiler

import slick.ast._
import slick.compiler.{WellTyped, Phase, CompilerState}
import Util._
import TypeUtil._

/**
 * Created by adam on 19.07.15.
 */

/**
 * Expand document into
 */
class ExpandDocument extends Phase {
  val name = "expandTables"

  def apply(state: CompilerState) = state.map { n => ClientSideOp.mapServerSide(n) { tree =>



// very ugly way to do that


    val f= tree.collect({case f:Filter=> f})(0).infer()

    val forInfer =  f.collect({case  TableExpansion(g,t,(c :@ n)) => TableExpansion(g,t :@ t.nodeType.replace({case CollectionType(cons,NominalType(sym,UnassignedType))=> CollectionType(cons,NominalType(sym,n))}) ,c ) })(0)
val tableWithType:Node = forInfer match {case TableExpansion(g,t,c) => t}

  val weWillSee  =  f.replace({case t:TableExpansion => tableWithType}).infer()

    val trr  = tree.replace({case f:Filter => weWillSee })


    val tyty = trr.collect({case Bind(g,f :@ n,s ) =>n })

   val ttts= tyty.map{case CollectionType(c,t) => t}


 val next =   trr.replace({case Ref(a) if tyty.contains(a) => Ref(a) :@ ttts(0)  })

    val nextInfer= next.infer()






val bbb = nextInfer.asInstanceOf[Bind]
    ResultSetMapping(new AnonSymbol,bbb.from,bbb.select).infer()

  }


  }
}
