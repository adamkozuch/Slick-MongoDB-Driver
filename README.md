[![Build Status](https://travis-ci.org/adamkozuch/Slick-MongoDB-Driver.svg?branch=master)](https://travis-ci.org/adamkozuch/Slick-MongoDB-Driver)
# Slick-MongoDB-Driver (experimental)
Slick driver for mongodb. Continuation of a https://github.com/dvinokurov/slick/tree/tmp/mongodb project.
In this project we have developed Document data structure which enables mapping between mongodb collection and 
Scala client side code.
# Slick version
3.1.0-M1
Following features are implemented: 
## performing insert to collection to with nested documents
## performing insert of array to collection
## querying mongo collection with (now result is in raw form and there is ongoing work on converter to slick type)
Features to be implemented:
## converter from casbah type to slick type

Usage
TODO compy unit test
In order to use driver we need to do following imports in our project
```
import slick.mongodb.lifted.MongoDriver.api._
import slick.lifted.{ProvenShape, Tag}
import slick.mongodb.types.doc
```
Defining documents
Below I present sample usage of implemented features.
```
case class first(x: Int, secondLevel: second, y: IndexedSeq[String]) extends doc.NewTerm
case class second(c: Int, thirdLevel: third1, thirdLevel2:third2) extends doc.NewTerm
case class third(c: Int, d: IndexedSeq[Int]) extends doc.NewTerm
```
For the client code we define case classes. Extending it with doc.NewTerm is a boilerplate
which is expected to be removed in the future.
Secondly defining of mapping is following
```
class firstLevelDocument(tags:Tag) extends Document[first](tags,"firstLevelDocument") {

  def x1 = field[Int]("primitiveFieldFirstLevel")
  def secondDoc = doc[secoundLevelDocument](tags)
  def arrOfString = array(field[String]("y"))
  def * = (x1, secondDoc, arrOfString) <> (first.tupled, first.unapply)
}

class secoundLevelDocument(tag:Tag) extends SubDocument[second](tag,"secoundLevelDocument") {
  type previousDocument = firstLevelDocument
  def x2 = field[Int]("primitiveFieldSecundLevel")
  def thirdDoc1 = doc[thirdLevelDocument1](tag)
  def thirdDoc2 = doc[thirdLevelDocument2](tag)
  def * = (x2, thirdDoc1, thirdDoc2) <> (second.tupled, second.unapply)
}

class thirdLevelDocument(tag:Tag) extends SubDocument[third](tag,"thirdLevelDocument") {
  type previousDocument = secoundLevelDocument
  def x3 = field[Int]("primitiveFieldThirdLevel")
  def arrOfInt = array(field[Int]("primitiveInt"))
  def * = (x3, arrayOfFurthDoc) <> (third1.tupled, third1.unapply)
}
```

First difference is that we use data type Document instead of Table. This is top level
data structure that representing collection in mongo. Next we are using class SubDocument 
which representing nested documents in mongo. Also mapping works for collections.
