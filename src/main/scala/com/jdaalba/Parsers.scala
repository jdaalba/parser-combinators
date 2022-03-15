package com.jdaalba

import com.jdaalba.Parsers.ParserOps.{skipL, skipR}
import scalaz.Functor

import scala.language.{implicitConversions, postfixOps}

object Parsers {

  type ParseResult[+A] = Option[(A, String)]
  type Parser[+A] = String => ParseResult[A]

  def parseChar(c: Char): Parser[Char] = parseString(c toString) map (_ charAt 0)

  def parseString(s: String): Parser[String] =
    input => if (input startsWith s) Some(s, input substring s.length) else None

  // implicit constructors
  implicit def char(c: Char): Parser[Char] = parseChar(c)

  implicit def token(s: String): Parser[String] = parseString(s)

  // syntax sugar
  implicit def asParserOps[A](p: Parser[A]): ParserOps[A] = ParserOps(p)

  case class ParserOps[A](p: Parser[A]) {
    def map[B](f: A => B): Parser[B] = ParserOps.map(p)(f)

    def |(p2: Parser[A]): Parser[A] = ParserOps.or(p, p2)

    def */>[B](pl: Parser[B]): Parser[B] = skipL(p)(pl)

    def <\*(pr: Parser[Any]): Parser[Any] = skipR(p)(pr)
  }

  // parser as Functor
  object ParserOps extends Functor[Parser] {
    override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] =
      fa andThen (r => r map (t => (f(t._1), t._2)))

    def or[A](p1: Parser[A], p2: Parser[A]): Parser[A] = s => p1(s).orElse(p2(s))

    def skipL[A](p1: Parser[Any])(p2: Parser[A]): Parser[A] = p1(_) flatMap { case (_, s) => p2(s) }

    def skipR[A](p1: Parser[A])(p2: Parser[Any]): Parser[A] =
      p1(_) flatMap { case (r, s) => p2(s) flatMap { case (_, s2) => Some(r, s2) } }
  }
}