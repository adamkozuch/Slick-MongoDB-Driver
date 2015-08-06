package slick.mongodb.compiler

import slick.ast.ClientSideOp
import slick.compiler.{CompilerState, Phase}

/**
 * Created by adam on 06.08.15.
 */
class fixingReferences extends Phase {
  val name = "expandTables"

  def apply(state: CompilerState) = state.map { n => ClientSideOp.mapServerSide(n) { tree =>

tree
  }


  }
}
