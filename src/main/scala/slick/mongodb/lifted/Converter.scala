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
import slick.mongodb.types.doc

/**
 * Created by adam on 26.08.15.
 */
class Converter[R](t:Type) {
  //
  case class second(c: Int, thirdLevel2:third2) extends doc.NewTerm
  case class third1(c: Int, fourthLevel: fourth) extends doc.NewTerm
  case class third2(c: Double, s: String) extends doc.NewTerm
  case class fourth(c: Int, d: IndexedSeq[Int]) extends doc.NewTerm

  val converter: GetResult[R] ={ GetResult[R](r => {
    val indexedSeqLike = r.toIndexedSeq

    // this may not be needed if I figure out way with json
    def fun (text:String, v:String): Any = {
      if (text == "Double") {
        v.toDouble
      } else if(text == "Int") {
        v.toInt
      } else {
        Double
      }
    }

    // converion of array to tuple we are getting arguments as array
    def toTuple(seq: Seq[_]): Product = {
      val clz = Class.forName("scala.Tuple" + seq.size)
      clz.getConstructors()(0).newInstance(seq.map(_.asInstanceOf[AnyRef]): _*).asInstanceOf[Product]
    }

def valuses(obj:MongoDBObject): List[AnyRef] = obj.values.toList.map(x => x match {
  case x:MongoDBObject => valuses(x)
  case n:BasicDBObject  => decoupleBasicObject(n)
  case y => y
})
    def decoupleBasicObject(obj:DBObject): List[AnyRef] = obj.toMap().values().toArray.toList.map(x => x match {
      case z: DBObject => decoupleBasicObject(z)
      case x => x
    })

    val decoupled = valuses(r)

    def partialyTupleTransformed(list:List[_]):Any = list.map(x => x match {
      case y:List[_] => toTuple(partialyTupleTransformed(y).asInstanceOf[Seq[_]])
      case a => a
    })

    val transformed = partialyTupleTransformed(decoupled) //this has to be transformed to my case classes


    def json = parse(r.toString()).getOrElse(Json.Null)

    //figure out why it doent working
//    println(decode[doc.NewTerm](json.noSpaces))

    val hc = json.findAllByKey("secoundLevelDocument")(0).hcursor
    Decoder.decodeJsonObject(hc) match  {
      case Right(ss) => ss.toMap.map(x => println(x._2 match {
        case i: List[_] =>   i
        case i:Json => println(Decoder.decodeJsonObject(i.hcursor).obj)
      }))
    }



   // transforming to case classes eventually do it afterwards(doing map on list and making tupled just before mapped to )

    val tt = Vector(1, Vector(11, "stringss"))

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

    // do this part with iteration
    def matchTypeWithValues(tp:Type, values:List[_]) = tp.children(0) match {

      case x: NominalType => x.structuralView match {
            // this is not recursive
        case y: MappedScalaType =>  y.structural.children.map(x => x match {

          case n: ScalaNumericType[_]  => n
          case p: ProductType => {
            val xxx = processProductType(p, values)
            val mappped = y.mapper.toMapped(xxx)
            mappped
          }
        })
      }
    }
    def processProductType(p:ProductType, values:List[_]):Any  = {
      val deepConverted = for (idx <- 0 to p.elements.length - 1) yield

        p.elements(idx) match {
          case x: MappedScalaType => {
            if (allTypesPrimitives(x)) {
              x.mapper.toMapped(toTuple(values(idx).asInstanceOf[List[_]]))
            } else {
              processProductType(x.asInstanceOf[ProductType], values(idx).asInstanceOf[List[_]])
            }
          }
          case y =>  values(idx)
        }
      toTuple(deepConverted)
    }


//    val some = myType.baseType.children
//    //primitive leave it as it is
//
//    val endType  = myType.mapper.toMapped((1,(10.0, "some info"))) // error here try to make recursion
//    val convertedTypes = myType.children(0).children.map(x => fun(x.toString, "22")) // here one after another

//    val endType = myType.mapper.toMapped(toTuple(convertedTypes))
//    endType.asInstanceOf[R]
   val tttt = matchTypeWithValues(t, decoupled(1).asInstanceOf[List[_]]).asInstanceOf[R]
    tttt
  })
  }
}
