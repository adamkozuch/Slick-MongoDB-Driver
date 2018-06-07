[![Build Status](https://travis-ci.org/adamkozuch/Slick-MongoDB-Driver.svg?branch=master)](https://travis-ci.org/adamkozuch/Slick-MongoDB-Driver)
# Slick-MongoDB-Driver (experimental)
Slick driver for mongodb. Continuation of a https://github.com/dvinokurov/slick/tree/tmp/mongodb project.
In this project we have developed Document and SubDocument data structures which enables mapping between mongodb collection and  Scala client-side code.
Underneath driver uses Slick version 3.2.3 and latest Cashbah version.


Supported features: 
  - insert and query of top-level arrays of primitive values
  - insert and query of top-level primitive values
  - insert and query of nested documents
  - insert and query of nested primitive values
  - insert and query of nested arrays of primitive values
  - filtering results base on predicate

Features to be implemented:
  - collections of documents
  - sorting 
  - limit 
  - switch to scala-mongodb-driver
  - other features as needed after discussion with community

Usage
In order to use driver we need to do following imports in our project
```
import slick.mongodb.lifted.MongoDriver.api._
import slick.lifted.{ProvenShape, Tag}
```
Defining documents 
Below presented sample usage of implemented features. First we have to define 
client side case classes.
```
case class top(x: Int, secondLevel: second, arr: IndexedSeq[Double])
case class nested(nestedArray: IndexedSeq[String], nestedPrimitive: Int)
```
Secondly we have to define documents.
```
class topLevel(tags: Tag) extends Document[top](tags, "collectionName") {
  def x1 = field[Int]("primitiveFieldFirstLevel")

  def secondDoc = doc[secoundLevelDocument](tags)

  def arrOfDouble = array(field[Double]("doubleArray"))

  def * = (x1, secondDoc, arrOfDouble) <> (top.tupled, top.unapply) // tutaj chodzi o to że dokument jest nested
}


class secoundLevelDocument(tag: Tag) extends SubDocument[second](tag, "secoundLevelDocument") {
  def x2 = field[Int]("primitiveFieldSecundLevel")  

  def arrOfInt = array(field[String]("nestedArrayField"))

  def nestedPrimitive = field[Int]("nestedPrimitive")

  def * = (x2, thirdDoc) <> (nested.tupled, second.unapply)
}
```
Usage of all currently supported fatures you can find in test file: https://github.com/adamkozuch/Slick-MongoDB-Driver/blob/update/src/test/scala/test.scala

The first difference compares to relational Slick is that we use datatype Document instead of Table. This is top-level
data structure that representing collection in mongo. Next, we are using class SubDocument which representing nested documents in mongo. We use 'field' method for defining primitive values in our mapping. 
We use 'array' for defining collections of primitives. 'doc' is used for defining nested documents in mapping.

For now project can be only used as a Github dependency.

The project is in the very early stage. If you happen to use it please write an issue or give me some feedback about it.
