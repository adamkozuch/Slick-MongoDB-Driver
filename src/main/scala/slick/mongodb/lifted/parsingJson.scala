package slick.mongodb.lifted

object parsingJson {
  import cats.syntax.either._
  import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

    val json =
      """
      {
        "comment":{"s":"something"}
      }
    """

    def run : Json= parse(json).getOrElse(Json.Null)


  sealed trait Foo
  case class Qux(ii: Int, dd: Option[Double]) extends Foo
  case class Bar(xs: Vector[String],q:Foo) extends Foo


  //one problem is mismatch between name in mongo and name of case classes
  // I should try to decode without going into parsing each element separately

  def main(args: Array[String]): Unit = {
    val foo: Foo = Qux(13, Some(14.0))
    val decoded  =  Decoder.decodeJson(run.hcursor)

    val baar:Foo = Bar(Vector("adam", "Lukasz"), foo )

    // this can solve problem of mismatch
    Decoder.forProduct2("ii", "dd")(Qux.apply)


    val json1 = foo.asJson.noSpaces

    val json2 = baar.asJson.noSpaces

    println(json1)
    println(json2)

    val decodebarr = decode[Foo](json2)
    println("baar" +  decodebarr)

    val decodedFoo = decode[Foo](json1)
    println(decodedFoo)

    val hc  = run.hcursor
    val res = hc.keys.get.map(x => hc.downField(x).downField("s").as[String](Decoder.decodeString))

    val is = res.map(x => x match {
      case Right(z) => z
    })

    print(is)
  }

  //res0: String = {
  //  "comment" : "something",
  //  "lat" : 10.12312421,
  //  "price" : 123,
  //  "third" : "normal"
  //}

}
