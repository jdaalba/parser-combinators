package com.jdaalba

import scalaz.Functor

import scala.language.postfixOps

object Parsers {

  type ParseResult[+A] = Option[(A, String)]
  type Parser[+A] = String => ParseResult[A]

  def parseChar(c: Char): Parser[Char] = ParserOps.map(parseString(c toString))(_ charAt 0)

  def parseString(s: String): Parser[String] =
    input => if (input startsWith s) Some(s, input substring s.length) else None

  object ParserOps extends Functor[Parser] {
    override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] =
      fa andThen (r => r map (t => (f(t._1), t._2)))
  }
}