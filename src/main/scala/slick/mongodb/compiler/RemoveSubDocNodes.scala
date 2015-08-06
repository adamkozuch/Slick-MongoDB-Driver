package slick.mongodb.compiler

import slick.ast._
import slick.compiler.{WellTyped, Phase, CompilerState}
import Util._
import TypeUtil._
import
slick.mongodb.SubDocNode

/**
 * Created by adam on 05.08.15.
 */
class RemoveSubDocNodes extends Phase {
val name = "removeSubDocNodes"

def apply(state: CompilerState) = state.map { n => ClientSideOp.mapServerSide(n) { tree =>
  val tree2 = tree.replace({case SubDocNode(s,n) =>n}, bottomUp = true)
  tree2
}}}
