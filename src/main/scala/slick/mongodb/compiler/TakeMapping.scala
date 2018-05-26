package slick.mongodb.compiler

import slick.ast._
import slick.compiler.{CompilerState, Phase}
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
    val projection = state.tree match {
      case b:Bind => {
        val mapping =  b.select match {
          case Pure(t:TableNode,_) =>  {
            val tpe2 = state.tree.collect({case t:TableExpansion =>t.columns })(0)
            val tpe3 = Pure(tpe2.replace({ case SubDocNode(s, n, p,ss) => ss }, bottomUp = true)).infer()
            tpe3
          }
          case Pure(s:SubDocNode,_)  => {
            b.select.replace({ case SubDocNode(s, n, p,ss) => ss }, bottomUp = true).infer()
          }
          case p:Pure => p.infer()
        }
        mapping
      }
      case t:TableExpansion => Pure(t.columns).infer()
    }

    val tpe = projection.nodeType
    val s = state.withNode(state.tree)   + (this -> tpe)
    s
  }
}