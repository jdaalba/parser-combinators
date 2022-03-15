package com.jdaalba

import scala.language.postfixOps

object Parsers {

  type ParseResult[+A] = Option[(A, String)]
  type Parser[+A] = String => ParseResult[A]

  def parseChar(c: Char): Parser[Char] = s => s.headOption match {
    case Some(c1) if c1 == c =>Some((c1, s.tail))
    case _ => None
  }
}