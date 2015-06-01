package slick.mongodb.direct

object MongoInterpolation {

  class ParametersKeeper[P](val parameters: P) extends Iterable[Any] {
    override def iterator: Iterator[Any] = new QuotingStringIterator(parameters match {
      case iterable:Iterable[Any] => iterable.iterator
      case product: Product => List.range(0, product.productArity).map(product.productElement).iterator
      case a: Any => List(a).iterator //TODO: unnecessary list creation
    })
    class QuotingStringIterator(val underlying: Iterator[Any]) extends Iterator[String] {
      override def hasNext: Boolean = underlying.hasNext
      override def next(): String = underlying.next() match{
        case n: Number => n.toString
        case o => "\""+o+"\""
      }
    }
  }

  // TODO: check if it's possible to refactor in order to avoid iterating through all the query by a single character
  def interpolate[P](query: String, parameters: P): String = {

    val NO_QUOTES = 0
    val IN_SINGLE_QUOTE = 1
    val IN_DOUBLE_QUOTE = 2

    val params = new ParametersKeeper(parameters).iterator
    val sb = new StringBuilder
    val tokensIterator = query.iterator
    var state = NO_QUOTES

    while(tokensIterator.hasNext){
      val char = tokensIterator.next()

      if(char=='\'' && state==NO_QUOTES) state=IN_SINGLE_QUOTE
      else if(char=='\'' && state==IN_SINGLE_QUOTE) state=NO_QUOTES
      else if(char=='"' && state==NO_QUOTES) state=IN_DOUBLE_QUOTE
      else if(char=='"' && state==IN_DOUBLE_QUOTE) state=NO_QUOTES

      if(char=='?' && state==NO_QUOTES){
        if(params.hasNext) sb.append(params.next())
        else throw new IllegalStateException("Number of parameters is less than required by query")
      }else sb.append(char)
    }

    sb.toString()
  }

}
