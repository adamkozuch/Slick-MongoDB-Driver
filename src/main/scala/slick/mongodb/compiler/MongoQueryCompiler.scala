package slick.mongodb.compiler

import slick.compiler.{Phase, QueryCompiler}

/**
 * Created by adam on 18.08.15.
 */
object MongoQueryCompiler {

  val phases = Vector(new TakeMapping ,MongoPhase.removeSubDocnodes,Phase.assignUniqueSymbols,MongoPhase.rebuildColumns,MongoPhase.expandDocument, Phase.verifySymbols,Phase.verifyTypes, MongoPhase.mergeToMongoQueryNode)

  val standard = new QueryCompiler(phases)

  object MongoPhase
  {
    val removeSubDocnodes = new RemoveSubDocNodes
    val rebuildColumns = new AddDynamics
    val expandDocument = new ExpandDocument
    val mergeToMongoQueryNode = new MergeToMongoQueryNode
  }
}
