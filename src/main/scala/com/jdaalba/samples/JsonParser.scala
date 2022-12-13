package com.jdaalba.samples

import com.jdaalba.core.Parsers
import com.jdaalba.core.Parsers._

object JsonParser {

  def parse: Parser[JToken] = parseBoolean | parseNumber | parseString

  private def parseNumber: Parser[JToken] = parseWhile(_ matches "\\d+\\.?\\d*").map(s => JNumber(s.toDouble))

  private def parseString: Parser[JToken] = between("\"").map(s => JString(s"$s"))

  private def parseBoolean: Parser[JToken] = (token("true") | "false").map(b => JBoolean(b == "true"))
}

sealed trait JToken

case class JNumber(d: Double) extends JToken

case class JString(s: String) extends JToken

case class JBoolean(b: Boolean) extends JToken
