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

    /** Make new Select part of TableExpansion columns */
    def addSelect(n: Node, selectPath: List[TermSymbol], nodeToAdd: Node): Node = n match {
        case sn: StructNode => {
          val count = for (i <- sn.elements if (i._1 == selectPath.head)) yield i
          if (count.isEmpty) sn.copy(elements = sn.elements :+ addElement(selectPath, nodeToAdd))
           else
            StructNode(sn.elements.map { case (t, n) => if (t == selectPath.head) (t, addSelect(n, selectPath.tail, nodeToAdd)) else (t, n) })
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


    //todo find cleaner way to searching for selects that are no in projection and are called dynamically
    //collect all selects from tree
    val selectsFromTree = tree.collect({ case s: Select if s.nodeType.isInstanceOf[ScalaBaseType[_]] => s }).toSet
    val tableExpansion = tree.collect({ case t: TableExpansion => t })(0)
    val selectsFromTableExpansion = tableExpansion.collect({ case s: Select if s.nodeType.isInstanceOf[ScalaBaseType[_]] => s }).toSet


    //remove reference symbol. We looking for Selects that are same so I remove reference symbol and check if Selects have same path and type
    val starProjectionSelects = selectsFromTableExpansion.map(t => Path(FwdPath.unapply(t).get.tail) :@ t.nodeType).toSet
    val dynSelec = selectsFromTree.map(t => Path(FwdPath.unapply(t).get.tail) :@ t.nodeType).toSet

    val dynamicSelects = (dynSelec -- starProjectionSelects).toSeq



    val columns = if(dynamicSelects.nonEmpty) addNodes(tableExpansion.columns, dynamicSelects.toList) else tableExpansion.columns

    val tree3 = tree.replace({ case TableExpansion(g, t, c) => TableExpansion(g, t, columns) })


    /** In the end I completely remove SubDocNode and keep only Select from it.  */
    val tree4 = tree3.replace({ case SubDocNode(s, n, p) => p }, bottomUp = true)


    tree4
  }

  }


}

