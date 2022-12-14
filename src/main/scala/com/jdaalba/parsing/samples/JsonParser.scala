package com.jdaalba.parsing.samples

import com.jdaalba.parsing.Parsers.Parser

import scala.language.{implicitConversions, postfixOps}

object JsonParser extends Parser[JToken] {

  import com.jdaalba.parsing.Parsers._

  override def apply(string: String): ParseResult[JToken] = (jnull <|> jboolean <|> jnumber <|> jstring)(string)

  private def jnull: Parser[JToken] = tok("null").map(_ => JNull)

  private def jboolean: Parser[JToken] = ("true" <|> "false") map (JBoolean compose (_ toBoolean))

  private def jnumber: Parser[JToken] = matches(_ isDigit) map (JNumber compose (_ toInt))

  private def jstring: Parser[JToken] = '"' *> matches(_ != '"') <* '"' map JString
}

sealed trait JToken

case object JNull extends JToken

case class JBoolean(b: Boolean) extends JToken

case class JNumber(i: Int) extends JToken

case class JString(s: String) extends JToken
