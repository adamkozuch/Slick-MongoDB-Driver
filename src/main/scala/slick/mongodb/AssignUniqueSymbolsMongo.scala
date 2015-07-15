package slick.mongodb

import slick.compiler.{CompilerState, Phase}

import scala.collection.mutable.HashSet
import slick.SlickException
import slick.ast._
import TypeUtil._


/**
 * Created by adam on 14.07.15.
 */
class AssignUniqueSymbolsMongo extends Phase {
  val name = "assignUniqueSymbols"

  def apply(state: CompilerState) = state.map { tree =>
    val seen = new HashSet[AnonSymbol]
    val seenType = new HashSet[TypeSymbol]
    var identitySymbolMap:Map[String,AnonTableIdentitySymbol] = Map()
    def tr(n: Node, replace: Map[AnonSymbol, AnonSymbol]): Node = {
      val n2 = n match { // Give TableNode and Pure nodes a unique TypeSymbol
        case t: TableNode =>
          //condition prevent assigning different symbols to same TableNode
          if(identitySymbolMap.keySet.contains(t.tableName))
          {
          t.copy(identity = identitySymbolMap(t.tableName))
          }
          else {
            val identitySymbol = new AnonTableIdentitySymbol
            identitySymbolMap +=(t.tableName ->identitySymbol)
            t.copy(identity = identitySymbol)
          }

        case p @ Pure(value, ts) =>
          if(seenType contains ts) Pure(value)
          else {
            seenType += ts
            p
          }
        case n => n
      }
      val n3 = // Remove all NominalTypes (which might have changed)
        if(n2.nodeHasType && !(n2.nodeType.collect { case _: NominalType => () }).isEmpty)
          n2.nodeUntypedOrCopy
        else n2
      n3 match {
        case r @ Ref(a: AnonSymbol) => replace.get(a) match {
          case Some(s) => if(s eq a) r else Ref(s)
          case None => r
        }
        case d: DefNode =>
          var defs = replace
          d.nodeMapScopedChildren { (symO, ch) =>
            val r = tr(ch, defs)
            symO match {
              case Some(a: AnonSymbol) =>
                if(seen.contains(a)) defs += a -> new AnonSymbol
                else seen += a
              case _ =>
            }
            r
          }.nodeMapGenerators {
            case a: AnonSymbol => defs.getOrElse(a, a)
            case s => s
          }
        case n => n.nodeMapChildren(tr(_, replace))
      }
    }
    tr(tree, Map())
  }
}