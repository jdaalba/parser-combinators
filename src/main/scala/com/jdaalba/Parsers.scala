package com.jdaalba

import scala.language.postfixOps

object Parsers {

  type ParseResult[+A] = Option[(A, String)]
  type Parser[+A] = String => ParseResult[A]

  def parseChar(c: Char): Parser[Char] = map(parseString(c toString))(_ charAt 0)

  def parseString(s: String): Parser[String] =
    input => if (input startsWith s) Some(s, input substring s.length) else None

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = p andThen (r => r map (t => (f(t._1), t._2)))
}