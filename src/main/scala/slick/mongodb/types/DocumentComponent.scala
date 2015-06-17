package slick.mongodb.types

import slick.ast._
import slick.lifted._
import slick.mongodb.lifted.MongoDriver
import slick.profile.RelationalTableComponent


/**
 * Created by adam on 02.06.15.
 */
trait DocumentComponent extends RelationalTableComponent {
  driver: MongoDriver =>

  abstract class Document[A](_documentTag: Tag, _documentName: String)
    extends Table[A](_documentTag, _documentName) {

    def document[T](documentName: String)(x: T)(implicit ol: OptionLift[T, Rep[Option[T]]]): Rep[Option[T]] = {
      val docName = new Symbol {def name = documentName}
      //wrap the Option into MongoNode in order to keep the documentName
      ol.lift(x).encodeRef(MongoNode(ol.lift(x).toNode,docName))
    }
  }
}


//exactly like Select we encode here values of MongoObject and its name
final case class MongoNode(option: Node, documentName: Symbol) extends UnaryNode with SimplyTypedNode {
  type Self = MongoNode
  def child = option
  override def nodeChildNames = Seq("in")
  protected[this] def nodeRebuild(child: Node) = copy(option = child).nodeTyped(nodeType)
  override def getDumpInfo = Path.unapply(this) match {
    case Some(l) => super.getDumpInfo.copy(name = "Path", mainInfo = l.reverseIterator.mkString("."))
    case None => super.getDumpInfo
  }
  protected def buildType = option.nodeType.select(documentName)
}











