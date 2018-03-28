package slick.mongodb.compiler

import slick.ast._
import slick.compiler.{WellTyped, CompilerState, Phase}
import slick.mongodb.SubDocNode
import Util._
import TypeUtil._

/**
 * Created by adam on 24.08.15.
 */
/**Phase before removing SubDocNodes. Type of SubDocNode is a type of TypeMapping from SubDocNode */
class TakeMapping extends Phase{
  val name: String = "takeMapping"
  type State = Type
  def apply(state: CompilerState) = {
    val projection = state.tree.collect({case t:Bind =>t.select })
     val mapping =  if(projection.nonEmpty)
      {
        projection(0).replace({ case SubDocNode(s, n, p,ss) => ss }, bottomUp = true).infer()
      } else
      {
         state.tree.collect({case t:TableExpansion =>t.columns })(0).infer()
      }

    val tpe = mapping.nodeType
    state.withNode(state.tree)   + (this -> tpe)
  }
}
