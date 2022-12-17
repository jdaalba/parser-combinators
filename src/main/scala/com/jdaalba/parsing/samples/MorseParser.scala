package com.jdaalba.parsing.samples

import com.jdaalba.parsing.Parsers.Parser
import com.jdaalba.parsing.utils.StringMonoid

import scala.language.{implicitConversions, postfixOps}

object MorseParser extends Parser[String] {

  import com.jdaalba.parsing.Parsers._

  val codes: Map[String, Char] = Map(
    ".-" -> 'A',
    "-..." -> 'B',
    "-.-." -> 'C',
    "-.." -> 'D',
    "." -> 'E',
    "..-." -> 'F',
    "--." -> 'G',
    "...." -> 'H',
    ".." -> 'I',
    ".---" -> 'J',
    "-.-" -> 'K',
    ".-.." -> 'L',
    "--" -> 'M',
    "-." -> 'N',
    "---" -> 'O',
    ".--." -> 'P',
    "--.-" -> 'Q',
    ".-." -> 'R',
    "..." -> 'S',
    "-" -> 'T',
    "..-" -> 'U',
    "...-" -> 'V',
    ".--" -> 'W',
    "-..-" -> 'X',
    "-.--" -> 'Y',
    "--.." -> 'Z',
    "-----" -> '0',
    ".----" -> '1',
    "..---" -> '2',
    "...--" -> '3',
    "....-" -> '4',
    "....." -> '5',
    "-...." -> '6',
    "--..." -> '7',
    "---.." -> '8',
    "----." -> '9',
    ".-.-.-" -> '.',
    "--..--" -> ',',
    "..--.." -> '?',
    ".-..-." -> '"',
    "-..-." -> '/'
  )

  val letterParsers: Parser[String] =
    codes.toIndexedSeq sortWith {
      case ((c1, _), (c2, _)) => c1.length > c2.length
    } map {
      case (code, letter) => code replaceVal letter
    } reduce (_ <|> _) map (_ toString)

  val parserMonoid: ParserMonoid[String] = ParserMonoid(StringMonoid)

  override def apply(s: String): ParseResult[String] = s match {
    case "" => ("", "")
    case _ => parserMonoid.append(parseToken, apply) $ s
  }

  def parseToken: Parser[String] = space <|> letter

  def space: Parser[String] = "  " replaceVal " "

  def letter: Parser[String] = letterParsers <*? " "
}
