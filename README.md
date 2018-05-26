[![Build Status](https://travis-ci.org/adamkozuch/Slick-MongoDB-Driver.svg?branch=master)](https://travis-ci.org/adamkozuch/Slick-MongoDB-Driver)
# Slick-MongoDB-Driver (experimental)
Slick driver for mongodb. Continuation of a https://github.com/dvinokurov/slick/tree/tmp/mongodb project.
In this project we have developed Document data structure which enables mapping between mongodb collection and  

Scala client side code.

Slick version  3.2.3

Following features are implemented: 
#### performing insert of document that contains nested documents
#### performing insert of document that contains arrays of primitives 
#### querying mongo collection

Usage
In order to use driver we need to do following imports in our project
```
import slick.mongodb.lifted.MongoDriver.api._
import slick.lifted.{ProvenShape, Tag}
import slick.mongodb.types.doc
```
Defining documents
Below I present sample usage of implemented features.
```
case class top1(x: Int, secondLevel: second, arr: IndexedSeq[Double]) extends doc.NewTerm
case class top2(secondLevel: second) extends doc.NewTerm
case class second(c: Int, thirdLevel1: third) extends doc.NewTerm
case class third(c: Double, s: IndexedSeq[String]) extends doc.NewTerm
```
For the client code we define case classes. Extending it with doc.NewTerm is a boilerplate
which is expected to be removed in the future.
Secondly defining of mapping is following
```
class topLevel1(tags: Tag) extends Document[top1](tags, "topLevel1") {
  def x1 = field[Int]("primitiveFieldFirstLevel")

  def secondDoc = doc[secoundLevelDocument](tags)

  def arrOfDouble = array(field[Double]("doubleArray"))

  def * = (x1, secondDoc, arrOfDouble) <> (top1.tupled, top1.unapply) // tutaj chodzi o to że dokument jest nested
}

class topLevel2(tags: Tag) extends Document[top2](tags, "topLevel2") {
  def secondDoc = doc[secoundLevelDocument](tags)

  def * = (secondDoc) <> (top2, top2.unapply) // tutaj chodzi o to że dokument jest nested
}

class secoundLevelDocument(tag: Tag) extends SubDocument[second](tag, "secoundLevelDocument") {
  def x2 = field[Int]("primitiveFieldSecundLevel")  

  def arrOfInt = array(field[Int]("PrimitiveFieldFourthLevelForArray"))

  def thirdDoc = doc[thirdLevelDocument](tag)

  def * = (x2, thirdDoc) <> (second.tupled, second.unapply)
}

class thirdLevelDocument(tag: Tag) extends SubDocument[third](tag, "thirdLevelDocument") {
  def x3 = field[Double]("primitiveFieldThirdLevel11")

  def x4 = array(field[String]("array of primitives"))

  def * = (x3, x4) <> (third.tupled, third.unapply)
}
```
For more I encourage you to look at the following test: https://github.com/adamkozuch/Slick-MongoDB-Driver/blob/update/src/test/scala/test.scala

First difference is that we use data type Document instead of Table. This is top level
data structure that representing collection in mongo. Next we are using class SubDocument 
which representing nested documents in mongo. Also mapping works for collections of primitives (in plans there is support for collections of documents).
