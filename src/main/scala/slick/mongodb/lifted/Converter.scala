package slick.mongodb.lifted

import slick.ast._
import slick.mongodb.direct.GetResult
import com.mongodb.{BasicDBList, BasicDBObject, DBObject}
import com.mongodb.casbah.commons.MongoDBObject
import slick.util.ConstArray

/**
 * Created by adam on 26.08.15.
 */
class Converter[R](t:Type) {
  val converter: GetResult[R] ={ GetResult[R](r => {
    def toTuple(seq: Seq[_]): Product = {
      val clz = Class.forName("scala.Tuple" + seq.size)
      clz.getConstructors()(0).newInstance(seq.map(_.asInstanceOf[AnyRef]): _*).asInstanceOf[Product]
    }

    def extractValues(obj:MongoDBObject): Vector[AnyRef] = obj.values.toVector.map(x => x match {
      case x:MongoDBObject => extractValues(x)
      case x :BasicDBList => x.toArray.toVector // maye I shouldn't cast it to vector
      case n:BasicDBObject  => decoupleBasicObject(n)
      case y => y
    })

    def decoupleBasicObject(obj:DBObject): Vector[AnyRef] = obj.toMap().values().toArray.toVector.map(x => x match {
      case dbl :BasicDBList => {
       val some = dbl.toArray.toVector
        some
      }
      case z: DBObject => decoupleBasicObject(z)
      case x => x
    })

    val decoupled = extractValues(r).tail // we skip treat it as a tuple

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

    def matchTypeWithValues(tp:Type, values:Any) = tp.children(0) match {

      case x: NominalType => x.structuralView match {
        case s:MappedScalaType => {
          val res = processProductType(s, values.asInstanceOf[Vector[_]])
          res
        }
        case n: ScalaNumericType[_]  => {
          def extract(v:Vector[_]):Any = v(0) match { // making generic for base types
            case v:Vector[_] => extract(v)
            case x => x
          }
          values match {
            case v:Vector[_] =>  extract(v)
            case x => x
          }
        }
        case c: CollectionType => {
          def extract(v:Vector[_]):Any = v(0) match { // making generic for base types
            case c:Vector[_] => extract(c)
            case x => v
          }
          values.asInstanceOf[Vector[_]](0) match {
            case v:Vector[_] => extract(v)
            case x => values
          }
        }
      }
      case c:CollectionType => c.elementType match {
        case x: NominalType => x.structuralView match {
          case s:MappedScalaType => {
            val res = processProductType(s, values.asInstanceOf[Vector[_]])
            res
          }
          case n: ScalaNumericType[_]  => values  // TODO making this case more generic
          case c: CollectionType  => values
        }

      }
    }

    def processProductType(y:MappedScalaType, values:Vector[_]): Any  = {
      y.structural.children(0) match {
        case p: ProductType => {
          val deepConverted = for (idx <- 0 to p.elements.length - 1) yield

            p.elements(idx) match {
              // iterate elements of product Mapped type or primitive
              case x: MappedScalaType => {
                if (allTypesPrimitives(x)) {
                  x.mapper.toMapped(toTuple(values(idx).asInstanceOf[Vector[_]]))
                } else {
                  processProductType(x, values(idx).asInstanceOf[Vector[_]])
                }
              }
              case y => {
                values.asInstanceOf[Vector[_]](idx) match {
                  case list: ConstArray[_] => list
                  case v => v
                }
              }
            }

          val tupled = toTuple(deepConverted)
          y.mapper.toMapped(tupled)
        }
        case c:MappedScalaType  => y.mapper.toMapped(processProductType(c, values))
      }
    }

    // decoupled is different depending on calling from top level and nested level TODO

    val tttt = {
      if (decoupled.length > 1) // TODO this condition is wrong
        matchTypeWithValues(t, decoupled).asInstanceOf[R]
      else
        matchTypeWithValues(t, decoupled(0)).asInstanceOf[R]  // TODO ugly hack change later
    }

    tttt
  })
  }
}
