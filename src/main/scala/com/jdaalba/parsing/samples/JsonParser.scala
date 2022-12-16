package com.jdaalba.parsing.samples

import com.jdaalba.parsing.Parsers.Parser

import scala.language.{implicitConversions, postfixOps}

object JsonParser extends Parser[JToken] {

  import com.jdaalba.parsing.Parsers._

  type JParser = Parser[JToken]

  override def apply(string: String): ParseResult[JToken] = parse(string)

  private def parse: JParser = jnull <|> jboolean <|> jnumber <|> jstring <|> jarray <|> jobject

  private def jnull: JParser = "null" replaceVal JNull

  private def jboolean: JParser = ("true" <|> "false") map (_ toBoolean) map JBoolean

  private def jnumber: JParser = matches(_ isDigit) map (_ toInt) map JNumber

  private def jstring: Parser[JString] = '"' *> matches(_ != '"') <* '"' map JString

  private def jarray: JParser = '[' *> arrayElementParser <* ']'

  private def arrayElementParser: Parser[JArray] = inp =>
    if (inp startsWith "]") {
      Some((JArray(), inp))
    } else {
      for {
        (elem, tail1) <- (JsonParser <*? ',')(inp)
        (list, tail2) <- arrayElementParser(tail1)
      } yield (JArray(elem) ++ list, tail2)
    }

  private def jobject: JParser = '{' *> objectElementParser <* '}'

  private def objectElementParser: Parser[JObject] = inp =>
    if (inp startsWith "}") {
      Some((JObject(), inp))
    } else {
      for {
        (label, tail1) <- (jstring <* ':').map(_.s)(inp) // parses key
        (value, tail2) <- (JsonParser <*? ',')(tail1) // parses value
        (jObj2, tail3) <- objectElementParser(tail2) // parses next tuple
      } yield (JObject(label -> value) ++ jObj2, tail3)
    }
}

sealed trait JToken

case object JNull extends JToken

case class JBoolean(b: Boolean) extends JToken

case class JNumber(i: Int) extends JToken

case class JString(s: String) extends JToken

case class JArray(js: List[JToken]) extends JToken {

  def ++(other: JArray): JArray = JArray(js ++ other.js)
}


object JArray {

  def apply(tokens: JToken*): JArray = JArray(tokens.toList)
}

case class JObject(entries: Map[String, JToken]) extends JToken {

  def ++(other: JObject): JObject = JObject(entries ++ other.entries)
}

object JObject {

  def apply(tuple: (String, JToken)*): JObject = JObject(tuple.toMap)
}
