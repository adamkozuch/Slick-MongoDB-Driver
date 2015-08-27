package slick.mongodb.compiler

import slick.ast._
import slick.compiler.{WellTyped, Phase, CompilerState}
import Util._
import TypeUtil._
import slick.mongodb.SubDocNode

/**
 * Created by adam on 12.08.15.
 */
/**
 * Purpose of that phase is to make all fields not in columns in TableExpansion part of it.
 * During that phase tree still contains SubDocNodes which can contain Selects that are needed for
 * building columns
 */
class AddDynamics extends Phase {
  val name = "rebuildColumns"

  def apply(state: CompilerState) = state.map { n => ClientSideOp.mapServerSide(n) { tree =>

    //todo simplify these methods
    /** Make new Select part of TableExpansion columns */
    def addSelect(subTree: Node, selectPath: List[TermSymbol], newNode: Node): Node = subTree match {
        case sn: StructNode => {
          val count = for (i <- sn.elements if (i._1 == selectPath.head)) yield i   // todo change that line
          if (count.isEmpty) sn.copy(elements = sn.elements :+ addElement(selectPath, newNode))
           else
            StructNode(sn.elements.map { case (t, n) => if (t == selectPath.head) (t, addSelect(n, selectPath.tail, newNode)) else (t, n) })
        }
        case s: Select => s
      }

      def addElement(symbols: List[TermSymbol], s: Node): (TermSymbol, Node) = {
        symbols match {
          case h :: Nil => (h, s)
          case h :: t => (h, StructNode(IndexedSeq((addElement(t, s)))))
        }
      }

    def addNodes(n: Node, selects: List[Node]): Node = {
      val tree2 = addSelect(n, Path.unapply(selects.head).get, selects.head)
      selects match {
        case l :: Nil => tree2
        case l :: s => addNodes(tree2, s)
      }
    }

    //For now I assume that will be one TableExpansion
    val tableExpansion = tree.collect({ case t: TableExpansion => t })(0)

    //remove reference symbol. We looking for Selects that are same so I remove reference symbol and check if Selects have same path and type
    val treePrimitivesWithoutRef = {
     val primitives =  tree.collect({case (s: Select) :@ (_: ScalaBaseType[_]) => s }).toSet
      primitives.map(t => Path(FwdPath.unapply(t).get.tail) :@ t.nodeType)
    }

    val tePrimitivesWithoutRef = {
     val primitives = tableExpansion.collect({case (s: Select) :@ (_: ScalaBaseType[_])  => s }).toSet
          primitives.map(t => Path(FwdPath.unapply(t).get.tail) :@ t.nodeType)
    }
    //If it isn't empty columns in TableExpansion will be changed
    val dynamicSelects = (treePrimitivesWithoutRef -- tePrimitivesWithoutRef).toSeq

    val tree3 = tree.replace({ case TableExpansion(g, t, c) =>{

      val columns = {
        if(dynamicSelects.isEmpty) c
        else
          addNodes(tableExpansion.columns, dynamicSelects.toList)
      }
      TableExpansion(g,t,columns)
    }  })

    /** In the end I completely remove SubDocNode and keep only Select from it.  */
     tree3.replace({ case SubDocNode(s, n, p,ss) => p }, bottomUp = true)
  }
  }
}

