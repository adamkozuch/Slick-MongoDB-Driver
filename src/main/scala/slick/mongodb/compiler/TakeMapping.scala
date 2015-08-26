package slick.mongodb.compiler

import slick.ast._
import slick.compiler.{WellTyped, CompilerState, Phase}
import slick.mongodb.SubDocNode
import Util._
import TypeUtil._

/**
 * Created by adam on 24.08.15.
 */
class NextPhase extends Phase{
  val name: String = "removeMappedTypes"

  type State = Type
  def apply(state: CompilerState) = {
    val projections =  state.tree.collect({case t:Bind =>t.select })(0).replace({ case SubDocNode(s, n, p,ss) => ss }, bottomUp = true).infer()
    val tpe = projections.nodeType

    state.withNode(state.tree)   + (this -> tpe)
  }

}
