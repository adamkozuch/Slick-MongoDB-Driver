package slick.mongodb.compiler

import slick.ast._
import slick.ast.Util._
import slick.compiler.{CompilerState, Phase}
import slick.util.{Ellipsis, ??}

/**
 * Created by adam on 27.08.15.
 */
class ShufflePhase extends Phase {
  type State = this.type
  val name: String = "shufflePhase"

  def apply(state: CompilerState) = state.map { n => ClientSideOp.mapServerSide(n) { tree => {

    // todo not working for now.
    /** Pull Bind nodes up to the top level through Filter and CollectionCast. */
//    def shuffle(n: Node): Node = n match {
//
//      case n@Filter(s1, from1, pred1) =>
//        shuffle(from1) match {
//          case from2@Bind(bs1, bfrom1, p: Pure) =>
//            logger.debug("Pulling Bind out of Filter", Ellipsis(n.copy(from = from2), List(0, 0)))
//            val s3 = new AnonSymbol
//
//            //what if in where I have Ref ??
//            val res = Bind(bs1, Filter(s3, bfrom1, pred1), p)
//            logger.debug("Pulled Bind out of Filter", Ellipsis(res, List(0, 0)))
//            res.infer()
//          case from2 =>
//            if (from2 eq from1) n else n.copy(from = from2) :@ n.nodeType
//        }
//      case n => n
//    }


    tree
  }
  }
  }
}