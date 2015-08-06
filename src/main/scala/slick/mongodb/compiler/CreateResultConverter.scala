package slick.mongodb.compiler

import slick.compiler.{CompilerState, Phase}

/**
 * Created by adam on 19.07.15.
 */
class CreateResultConverter extends Phase {
  type State = this.type

  def apply(state: CompilerState) = ???

  val name: String = "createResultConverter"
}
