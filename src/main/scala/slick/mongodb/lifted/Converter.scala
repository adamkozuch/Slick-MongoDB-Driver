package slick.mongodb.lifted

import slick.ast._
import slick.mongodb.direct.GetResult
import cats.syntax.either._
import com.mongodb.{BasicDBList, BasicDBObject, DBObject}
import com.mongodb.casbah.commons.MongoDBObject
import io.circe._
import io.circe.parser._
import io.circe._
import io.circe.generic.semiauto._
import slick.util.ConstArray

/**
 * Created by adam on 26.08.15.
 */
class Converter[R](t:Type) {
  val converter: GetResult[R] ={ GetResult[R](r => {
    val indexedSeqLike = r.toIndexedSeq

    def toTuple(seq: Seq[_]): Product = {
      val clz = Class.forName("scala.Tuple" + seq.size)
      clz.getConstructors()(0).newInstance(seq.map(_.asInstanceOf[AnyRef]): _*).asInstanceOf[Product]
    }

    def extractValues(obj:MongoDBObject): List[AnyRef] = obj.values.toList.map(x => x match {
      case x:MongoDBObject => extractValues(x)
      case x :BasicDBList => x.toArray
      case n:BasicDBObject  => decoupleBasicObject(n)
      case y => y
    })

    def decoupleBasicObject(obj:DBObject): List[AnyRef] = obj.toMap().values().toArray.toList.map(x => x match {
      case yy :BasicDBList => {
       val some =  yy.toArray.toIndexedSeq
        some
      }
      case z: DBObject => decoupleBasicObject(z)
        // here I need to deal also with lists
      case x => x
    })

    val decoupled = extractValues(r).tail // we skip id

    def partialyTupleTransformed(list:List[_]):Any = list.map(x => x match {
      case y:List[_] => toTuple(partialyTupleTransformed(y).asInstanceOf[Seq[_]])
      case a => a
    })


    def allTypesPrimitives(mappedScalaType: MappedScalaType): Boolean = {
      mappedScalaType.children(0).asInstanceOf[ProductType].elements.foreach(x => {
        x match {
          case ss: ScalaBaseType[_] => ss
          case sss: ScalaNumericType[_] => sss
          case c => return false
        }
      })
      return true
    }

    def matchTypeWithValues(tp:Type, values:List[_]) = tp.children(0) match {

      case x: NominalType => x.structuralView match {
        case s:MappedScalaType => {
          val res = processProductType(s, values)
          res
        }
        case n: ScalaNumericType[_]  => values(0)  // TODO making this case more generic
      }
    }

    def processProductType(y:MappedScalaType, values:List[_]): Any  = {
      y.structural.children(0) match {
        case p: ProductType => {
          val deepConverted = for (idx <- 0 to p.elements.length - 1) yield

            p.elements(idx) match {
                // iterate elements of product Mapped type or primitive
              case x: MappedScalaType => {
                if (allTypesPrimitives(x)) {
                  x.mapper.toMapped(toTuple(values(idx).asInstanceOf[List[_]]))
                } else {
                  processProductType(x, values(idx).asInstanceOf[List[_]])
                }
              }
              case y => { values.asInstanceOf[List[_]](idx) match {
                case list:ConstArray[_] => list
                case v => v
              }}
            }

          val tupled = toTuple(deepConverted)
          y.mapper.toMapped(tupled)
        }
      }
    }

    // decoupled is different depending on calling from top level and nested level TODO

    val tttt = {
      if (decoupled.length > 1)
        matchTypeWithValues(t, decoupled).asInstanceOf[R]
      else
        matchTypeWithValues(t, decoupled(0).asInstanceOf[List[_]]).asInstanceOf[R]  // TODO ugly hack change later
    }
    tttt
  })
  }
}
