package slick.mongodb.lifted

import slick.ast._
import slick.mongodb.direct.GetResult

import cats.syntax.either._
import io.circe._, io.circe.parser._

/**
 * Created by adam on 26.08.15.
 */
class Converter[R](t:Type) {
  //
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

    def json = parse(r.toString()).getOrElse(Json.Null)

    // get a key for a table
    val hc = json.findAllByKey("thirdLevelDocument2")(0).hcursor

    val myType: MappedScalaType = t.children(0).asInstanceOf[NominalType]
      .structuralView.asInstanceOf[MappedScalaType]
    val some =myType.baseType.children
    //primitive leave it as it is
    val endType  = myType.mapper.toMapped((1,(10.0, "some info"))) // error here try to make recursion
//    val convertedTypes = myType.children(0).children.map(x => fun(x.toString, "22")) // here one after another
//
//    val endType = myType.mapper.toMapped(toTuple(convertedTypes))
    endType.asInstanceOf[R]
  })
  }
}
