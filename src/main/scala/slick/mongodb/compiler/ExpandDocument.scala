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
  val name = "expandDocument"

  def apply(state: CompilerState) = state.map { n => ClientSideOp.mapServerSide(n) { tree =>

    val from: Node = tree match {
      case Bind(g, f, s) => f
      case a => a
    }
    val from2 = from.infer()

    /**
    Type from columns in TableExpansion is a type of Document
     */
    val structs = from2.collect {
      case TableExpansion(_, TableNode(_, _, sym, _, _), c) => (sym -> c.nodeType)
    }.toMap

    /**
    Correct type in from
     */
    val from3 = from2.replace({
      case TableExpansion(_, t, _) => t
      case n => n :@ n.nodeType.replace { case NominalType(tsym:TableIdentitySymbol, UnassignedType) => NominalType(tsym, structs(tsym)) }
    }, keepType = true, bottomUp = true)

    val tree2 = tree match {
      case Bind(g, f, s) => Bind(g, from3, s)
      case a => from3
    }

    /*
    After setting correct type in from infer type of whole Binding
     */
    val tree3 = tree2.infer(typeChildren = true)

    tree3

  }


  }
}
