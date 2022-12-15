package com.jdaalba.parsing.samples

import com.jdaalba.parsing.Parsers.Parser

import scala.language.{implicitConversions, postfixOps}

object JsonParser extends Parser[JToken] {

  import com.jdaalba.parsing.Parsers._

  type JParser = Parser[JToken]

  override def apply(string: String): ParseResult[JToken] = parse(string)

  private def parse: JParser = jnull <|> jboolean <|> jnumber <|> jstring <|> jarray <|> jobject

  private def jnull: JParser = tok("null").map(_ => JNull)

  private def jboolean: JParser = ("true" <|> "false") map (JBoolean compose (_ toBoolean))

  private def jnumber: JParser = matches(_ isDigit) map (JNumber compose (_ toInt))

  private def jstring: Parser[JString] = '"' *> matches(_ != '"') <* '"' map JString

  private def jarray: JParser = '[' *> arrayElementParser <* ']'

  private def arrayElementParser: JParser = inp => {

    def f(s: String): ParseResult[List[JToken]] = {
      if (s startsWith "]") return Some((Nil, s))
      ((',' <|> "") *> JsonParser)(s) match {
        case Some((jtoken, tail)) => f(tail) map (t => (jtoken :: t._1, t._2))
        case _ => None
      }
    }

    f(inp) map (t => (JArray(t._1), t._2))
  }

  private def jobject: JParser = '{' *> objectElementParser <* '}'

  private def objectElementParser: Parser[JObject] = {
    inp => {
      def f(s: String): ParseResult[Map[String, JToken]] = {
        if (s startsWith "}") {
          Some((Map(), s))
        } else {
          for {
            (label, s1) <- ((',' <|> "") *> jstring)(s)
            (_, s2) <- ':'(s1)
            (value, s3) <- JsonParser(s2)
            (m2, s4) <- f(s3)
          } yield (Map(label.s -> value) ++ m2, s4)
        }
      }
      f(inp) map (t => (JObject(t._1), t._2))
    }
  }
}

sealed trait JToken

case object JNull extends JToken

case class JBoolean(b: Boolean) extends JToken

case class JNumber(i: Int) extends JToken

case class JString(s: String) extends JToken

case class JArray(js: List[JToken]) extends JToken

object JArray {
  def apply(tokens: JToken*): JArray = JArray(tokens.toList)
}

case class JObject(entries: Map[String, JToken]) extends JToken

object JObject {

  def apply(tuple: (String, JToken)*): JObject = JObject(tuple.toMap)
}
