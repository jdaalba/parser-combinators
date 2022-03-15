package com.jdaalba

import scalaz.Functor

import scala.language.{implicitConversions, postfixOps}

object Parsers {

  type ParseResult[+A] = Option[(A, String)]
  type Parser[+A] = String => ParseResult[A]

  def parseChar(c: Char): Parser[Char] = parseString(c toString) map (_ charAt 0)

  def parseString(s: String): Parser[String] =
    input => if (input startsWith s) Some(s, input substring s.length) else None

  // syntax sugar
  implicit def asParserOps[A](p: Parser[A]): ParserOps[A] = ParserOps(p)

  case class ParserOps[A](p: Parser[A]) {
    def map[B](f: A => B): Parser[B] = ParserOps.map(p)(f)
  }

  // parser as Functor
  object ParserOps extends Functor[Parser] {
    override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] =
      fa andThen (r => r map (t => (f(t._1), t._2)))
  }
}