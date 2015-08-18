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

/**
 * SubDocNodes are created for representing nested documents. Here we remove it form columns in TableExpansion
 * and leave only StructNodes
 */
class RemoveSubDocNodes extends Phase {
val name = "removeSubDocNodes"

def apply(state: CompilerState) = state.map { n => ClientSideOp.mapServerSide(n) { tree =>
  val tableExpansion = tree.collect({case t:TableExpansion =>t })(0).replace({case SubDocNode(s,n,p) =>n}, bottomUp = true)
  val tree2 = tree.replace({case t:TableExpansion => tableExpansion})
  tree2
}}}
