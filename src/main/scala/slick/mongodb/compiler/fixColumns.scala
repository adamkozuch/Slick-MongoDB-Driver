package slick.mongodb.compiler

import slick.ast._
import slick.compiler.{WellTyped, Phase, CompilerState}
import Util._
import TypeUtil._
/**
 * Created by adam on 05.08.15.
 */
class fixColumns extends Phase {
val name = "expandTables"

def apply(state: CompilerState) = state.map { n => ClientSideOp.mapServerSide(n) { tree =>

// refactor ant try remove by fixing source
  val fff = tree.replace({case TableExpansion(g,tt,c) =>{


    val newColumns:IndexedSeq[(TermSymbol,Node)] = c.children.map(x => x match {
      case   ss:Select =>(ss.field,ss)
      case  StructNode(t) =>t(0)
      case   p:Pure => (new FieldSymbol("symbolkolekcji")(Seq(),UnassignedType),p)
    }).toIndexedSeq

    TableExpansion(g,tt,StructNode(newColumns))
  }})

  fff

}}}
