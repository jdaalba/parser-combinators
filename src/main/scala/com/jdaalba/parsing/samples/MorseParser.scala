package com.jdaalba.parsing.samples

import com.jdaalba.parsing.Parsers.Parser

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

  override def apply(v1: String): ParseResult[String] = letterParsers.map(_.toString) $ v1

  def letterParsers: Parser[Char] = codes.toIndexedSeq sortWith {
    case ((c1, _), (c2, _)) => c1.length > c2.length
  } map {
    case (code, letter) => code replaceVal letter
  } reduce (_ <|> _)
}
